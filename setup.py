from distutils.core import setup

with open('README.md') as f:
    readme = f.read()

setup(
    name='RKernel',
    version='1.0',
    packages=['rkernel'],
    description='A pythonic Jupyter kernel for R',
    long_description=readme,
    author='Martin Elff',
    author_email='martin@elff.eu',
    url='https://github.com/melff/RKernel',
    classifiers=[
        'Intended Audience :: Science/Research',
        'Intended Audience :: Developers',
        'Intended Audience :: Education',
        'License :: OSI Approved :: BSD License',
        'Programming Language :: Python :: 3',
        'Programming Language :: R',
        'Framework :: Jupyter',
    ],
)
