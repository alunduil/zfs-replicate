"""Test receive options flag rendering."""

import zfs.replicate.receive.type as sut


class TestOptionsToFlags:
    """Each enabled setting contributes its flag, and nothing else does."""

    def test_forces_by_default(self) -> None:
        """Default options render only -F."""
        assert sut.Options().to_flags() == ["-F"]

    def test_omits_force_when_disabled(self) -> None:
        """Disabling force renders no flags."""
        assert not sut.Options(force=False).to_flags()

    def test_adds_no_mount(self) -> None:
        """Enabling no_mount renders -u."""
        assert "-u" in sut.Options(no_mount=True).to_flags()

    def test_adds_resume(self) -> None:
        """Enabling resume renders -s."""
        assert "-s" in sut.Options(resume=True).to_flags()

    def test_renders_properties(self) -> None:
        """Each property renders a -o token followed by its KEY=VALUE token."""
        flags = sut.Options(force=False, properties={"readonly": "on", "canmount": "noauto"}).to_flags()

        assert flags == ["-o", "readonly=on", "-o", "canmount=noauto"]
