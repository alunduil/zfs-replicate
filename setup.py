# pylint: disable=missing-docstring

from setuptools import setup

with open("README.rst") as fh:
    README = fh.read()

setup(
    name="zfs-replicate",
    version="1.1.6",
    description="ZFS Snapshot Replicator",
    long_description=README,
    url="https://github.com/alunduil/zfs-replicate",

    author="Alex Brandt",
    author_email="alunduil@gmail.com",

    license="BSD-2",

    packages=[
        "zfs.replicate",
        "zfs.replicate.cli",
        "zfs.replicate.compress",
        "zfs.replicate.filesystem",
        "zfs.replicate.snapshot",
        "zfs.replicate.ssh",
        "zfs.replicate.task",
        ],

    setup_requires=[
        "pytest-runner",
        ],

    install_requires=[
        "click",
        "stringcase",
        ],

    tests_require=[
        "hypothesis",
        "pytest",
        ],

    entry_points = {
        "console_scripts": ["zfs-replicate=zfs.replicate.cli.main:main",],
        },
    )
