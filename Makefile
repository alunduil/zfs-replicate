PACKAGES=zfs
TEST_PACKAGES=zfs_test

.PHONY: all
all: lint check

.PHONY: check
check: test

.PHONY: test
test: clean
	find $(PACKAGES) $(TEST_PACKAGES) -name '*.py' -exec mypy --strict "{}" +
	pytest -x --doctest-modules $(PACKAGES:%=--cov=%) --cov-report term-missing

.PHONY: lint
lint: clean
	isort -y --atomic -rc $(PACKAGES) $(TEST_PACKAGES)
	black -l 120 --target-version py36 $(PACKAGES) $(TEST_PACKAGES)
	pylint -j 0 $(PACKAGES) $(TEST_PACKAGES)
	vulture --min-confidence=61 --exclude='zfs/replicate/cli/click.py' --ignore-names=main $(PACKAGES) $(TEST_PACKAGES)

.PHONY: clean
clean:
	find . -name '*.py[co]' -exec rm -f "{}" +
	find . -name '*~' -exec rm -f "{}" +
	find . -name '__pycache__' -exec rmdir "{}" +
	rm -rf build
	rm -rf dist
	rm -rf *.egg-info

