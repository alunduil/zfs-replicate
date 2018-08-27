PACKAGES=zfs
TEST_PACKAGES=zfs_test

.PHONY: all
all: lint check

.PHONY: check
check: test

.PHONY: test
test: clean
	find $(PACKAGES) -name '*.py' -exec mypy --strict "{}" +
	# TODO find $(TEST_PACKAGES) -name '*.py' -exec mypy --strict "{}" \;

	pytest

.PHONY: lint
lint: clean
	isort -y --atomic -rc $(PACKAGES) $(TEST_PACKAGES)
	black -l 120 --py36 $(PACKAGES) $(TEST_PACKAGES)

	pylint -j 0 $(PACKAGES)
	find $(TEST_PACKAGES) -name '*.py' -exec pylint -j 0 "{}" +

	vulture --exclude='zfs/replicate/cli/click.py' --ignore-names=main $(PACKAGES) $(TEST_PACKAGES)

.PHONY: clean
clean:
	find . -name '*.py[co]' -exec rm -f "{}" +
	find . -name '*~' -exec rm -f "{}" +
	rm -rf build
	rm -rf dist
	rm -rf *.egg-info

