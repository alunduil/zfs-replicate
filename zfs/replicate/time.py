"""Helper funcitons for dealing with time."""

import datetime

def round(dt: datetime.datetime) -> datetime.time: # pylint: disable=invalid-name,redefined-builtin
    """Round a `datetime` into a `time` with minute resolution."""

    if dt.second < 30 or dt.minute == 59:
        now = dt.replace(second=0)
    else:
        now = dt.replace(minute=dt.minute + 1, second=0)

    return datetime.time(now.hour, now.minute)
