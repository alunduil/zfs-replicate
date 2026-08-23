# How to work a surviving mutant

Kill a mutant the suite failed to catch, or account for one no test can kill.
This assumes you have picked up one of the mutation-testing issues and want a
module's survivors down to zero.

## Run the sweep

```bash
poetry install --with mutation
poetry run mutmut run --max-children 1
poetry run mutmut results
```

Allow a quarter of an hour for the whole tree. Pass `--max-children 1`: the
parallel runner mis-attributes child exit codes on this suite, so the same
mutant reports killed on one run and survived on the next.

Narrow later sweeps to the module you are working:

```bash
poetry run mutmut run --max-children 1 'zfs.replicate.snapshot.list.*'
```

## Read one survivor

Take a name from `mutmut results` and read the edit it stands for:

```bash
poetry run mutmut show zfs.replicate.snapshot.list.x__snapshot__mutmut_4
```

`mutmut browse` shows the same diffs in a terminal UI and re-tests a mutant in
place.

## Kill it with a test

When the edit changes what a user would see, write a test that fails on the
mutated behavior, following the [testing conventions]. Then confirm the mutant
dies:

```bash
poetry run mutmut run --max-children 1 zfs.replicate.snapshot.list.x__snapshot__mutmut_4
```

## Record an equivalent mutant

When no test can tell the edit apart from the original, leave it and give the
reason in a comment at the code. A message string no caller reads and a
`sorted()` call on an already-ordered sequence both land here.

## Exclude code `mutmut` can't reach

`mutmut` swaps a mutant in when the mutated function runs, so code that runs
while the module is first imported has already finished by then. Mark the
enclosing function and give the reason above it:

```python
def send_group(command: Callable[..., None]) -> Callable[..., None]:  # pragma: no mutate block
```

[`cli/options.py`] carries both Click option groups this way.

[`cli/options.py`]: ../../zfs/replicate/cli/options.py
[testing conventions]: ../reference/testing.md
