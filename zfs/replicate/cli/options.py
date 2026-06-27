"""Command-line option groups."""

import functools
from typing import Callable, Dict, Tuple

import click

from .. import receive


def receive_group(command: Callable[..., None]) -> Callable[..., None]:
    """Attach the ``--receive-*`` option group and collapse it to receive.Options.

    Keeps the receive-side flags defined together and namespaced under
    ``--receive-`` so they can't collide with the global and transport
    options, and hands the command a single ``receive_options`` argument
    rather than one parameter per flag.
    """
    command = click.option(
        "--receive-set",
        "receive_set",
        multiple=True,
        metavar="KEY=VALUE",
        help=(
            "Set a property on the received data set (maps to -o KEY=VALUE on"
            " zfs receive). Repeatable, for example --receive-set readonly=on"
            " --receive-set canmount=noauto."
        ),
    )(command)
    command = click.option(
        "--receive-resume-token-capable",
        "receive_resume",
        is_flag=True,
        help=(
            "Pass -s to zfs receive so an interrupted transfer saves a resume"
            " token on the destination."
        ),
    )(command)
    command = click.option(
        "--receive-mount/--receive-no-mount",
        "receive_mount",
        default=True,
        help=(
            "Mount received data sets (default). Use --receive-no-mount to pass"
            " -u so they are left unmounted."
        ),
    )(command)
    command = click.option(
        "--receive-force/--receive-no-force",
        "receive_force",
        default=True,
        help=(
            "Pass -F to zfs receive so the destination rolls back to match the"
            " stream (default). Use --receive-no-force to fail instead of"
            " discarding divergent destination snapshots."
        ),
    )(command)

    @functools.wraps(command)
    def wrapper(  # pylint: disable=R0913
        *args: object,
        receive_force: bool,
        receive_mount: bool,
        receive_resume: bool,
        receive_set: Tuple[str, ...],
        **kwargs: object,
    ) -> None:
        return command(
            *args,
            receive_options=receive.Options(
                force=receive_force,
                no_mount=not receive_mount,
                resume=receive_resume,
                properties=_parse_properties(receive_set),
            ),
            **kwargs,
        )

    return wrapper


def _parse_properties(properties: Tuple[str, ...]) -> Dict[str, str]:
    parsed: Dict[str, str] = {}

    for item in properties:
        key, separator, value = item.partition("=")
        if not separator or not key:
            raise click.BadParameter(
                f"expected KEY=VALUE, got {item!r}", param_hint="--receive-set"
            )
        parsed[key] = value

    return parsed
