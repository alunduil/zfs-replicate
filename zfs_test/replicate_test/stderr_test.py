"""zfs.replicate.stderr tests."""

import zfs.replicate.stderr as sut


class TestClean:
    """``clean`` strips the noise a remote shell adds around an error message."""

    def test_keeps_a_plain_message_intact(self) -> None:
        """Stderr carrying no noise survives unchanged."""
        assert (
            sut.clean(b"cannot destroy 'pool/data': dataset is busy") == b"cannot destroy 'pool/data': dataset is busy"
        )

    def test_drops_trailing_line_endings(self) -> None:
        """The line ending a shell appends does not reach the error message."""
        assert sut.clean(b"cannot open 'pool/data'\r\n") == b"cannot open 'pool/data'"

    def test_drops_the_none_cipher_warning(self) -> None:
        """The banner an unencrypted ssh session prints is not an error."""
        assert sut.clean(b"WARNING: ENABLED NONE CIPHER\r\n") == b""

    def test_keeps_a_message_the_none_cipher_warning_precedes(self) -> None:
        """The failure reported after the banner survives its removal."""
        assert sut.clean(b"WARNING: ENABLED NONE CIPHERcannot open 'pool/data'\n") == b"cannot open 'pool/data'"
