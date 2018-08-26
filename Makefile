PACKAGES=zfs
TEST_PACKAGES=zfs_test

.PHONY: check
check: test

.PHONY: test
test: clean develop
	find ${PACKAGES} ${TEST_PACKAGES} -name '*.py' -exec mypy --strict "{}" +
	pytest

.PHONY: clean
clean:
	find . -name '*.py[co]' -exec rm -f "{}" +
	find . -name '*~' -exec rm -f "{}" +
	rm -rf build
	rm -rf dist
	rm -rf *.egg-info

.PHONY: lint
lint: clean develop
	isort -y --atomic -rc $(PACKAGES) $(TEST_PACKAGES)
	pylint $(PACKAGES) $(TEST_PACKAGES)

.PHONY: develop
develop: .shadows/develop

.shadows/develop:
	pip install isort
	pip install mypy
	pip install -e .

	mkdir -p .shadows
	touch .shadows/develop

.PHONY: sdist
sdist:
	python setup.py sdist
