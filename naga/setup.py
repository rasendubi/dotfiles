#!/usr/bin/env python
from setuptools import setup

setup(
    name='naga',
    version='1.0',
    url='https://github.com/rasenduby/dotfiles',
    packages=['naga'],
    entry_points={
        'console_scripts': [
            'naga=naga:main',
        ],
    },
    license='MIT',
    install_requires=[
        'evdev',
    ],
)
