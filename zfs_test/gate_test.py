"""Temporary: fails so the CI results gate can be seen going red."""

import pytest


def test_gate_goes_red() -> None:
    """Fails unconditionally."""
    pytest.fail("deliberate failure to exercise the CI results gate")
