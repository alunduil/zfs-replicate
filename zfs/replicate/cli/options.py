"""Command-line option groups."""

import functools
from typing import Callable, Dict, Tuple

import click

from .. import receive, send


def send_group(command: Callable[..., None]) -> Callable[..., None]:
    """Add the ``--send-*`` flag group to a Click command.

    The decorated command must take a single ``send_options: send.Options``
    parameter and must not declare the ``--send-*`` flags itself -- this owns
    them and collapses them into that argument. Reach for this instead of
    listing the flags inline, so they stay grouped and the ``--send-`` prefix
    mirrors ``--receive-`` and keeps them from clashing with the global and
    transport options.
    """
    command = click.option(
        "--send-props",
        "send_props",
        is_flag=True,
        help=(
            "Pass -p to zfs send so the stream carries data set properties"
            " such as compression and recordsize."
        ),
    )(command)
    command = click.option(
        "--send-compressed",
        "send_compressed",
        is_flag=True,
        help=(
            "Pass -c to zfs send so already-compressed blocks replicate"
            " without being decompressed and recompressed."
        ),
    )(command)
    command = click.option(
        "--send-embed",
        "send_embed",
        is_flag=True,
        help=(
            "Pass -e to zfs send so embedded-data blocks (embedded_data"
            " feature) replicate without being re-read from disk."
        ),
    )(command)
    command = click.option(
        "--send-large-block",
        "send_large_block",
        is_flag=True,
        help=(
            "Pass -L to zfs send so blocks larger than 128 KiB replicate"
            " whole (requires the large_blocks feature on the destination)."
        ),
    )(command)
    command = click.option(
        "--send-raw/--send-no-raw",
        "send_raw",
        default=True,
        help=(
            "Pass -w (--raw) to zfs send so encrypted data sets replicate"
            " without decryption (default). Use --send-no-raw to send"
            " decrypted data, for example when the destination cannot"
            " preserve encryption."
        ),
    )(command)

    # Forwards every command parameter plus the five send flags, so the
    # argument count is inherent to collapsing them into one kwarg.
    @functools.wraps(command)
    def wrapper(  # pylint: disable=R0913
        *args: object,
        send_large_block: bool,
        send_raw: bool,
        send_embed: bool,
        send_compressed: bool,
        send_props: bool,
        **kwargs: object,
    ) -> None:
        return command(
            *args,
            send_options=send.Options(
                large_block=send_large_block,
                raw=send_raw,
                embed=send_embed,
                compressed=send_compressed,
                props=send_props,
            ),
            **kwargs,
        )

    return wrapper


def receive_group(command: Callable[..., None]) -> Callable[..., None]:
    """Add the ``--receive-*`` flag group to a Click command.

    The decorated command must take a single ``receive_options:
    receive.Options`` parameter and must not declare the ``--receive-*``
    flags itself -- this owns them and collapses them into that argument.
    Reach for this instead of listing the flags inline, so they stay
    grouped and the ``--receive-`` prefix keeps them from clashing with the
    global and transport options.
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

    # Forwards every command parameter plus the four receive flags, so the
    # argument count is inherent to collapsing them into one kwarg.
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
