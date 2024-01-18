from ipykernel.kernelapp import IPKernelApp
from . import RKernel

IPKernelApp.launch_instance(kernel_class=RKernel)
