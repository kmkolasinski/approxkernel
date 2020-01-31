from setuptools import setup, find_packages

VERSION = "0.0.7"


setup(
    name="approxkernel",
    version=VERSION,
    description="A library for approximating 2D kernels",
    url="...",
    author="Krzysztof Kolasinski",
    author_email="kmkolasinski@gmail.com",
    license="",
    packages=find_packages(exclude=["tests", "examples"]),
    include_package_data=True,
    zip_safe=False,
    install_requires=[
        "pytest==4.3.0",
        "numpy==1.16.4",
        "tensorflow==1.12.0",
        "matplotlib==3.0.0",
        "tqdm==4.26.0",
        "pyyaml==5.1",
    ],
    scripts=["scripts/fit_kernel"],
)
