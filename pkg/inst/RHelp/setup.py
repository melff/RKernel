import setuptools

setuptools.setup(
  name="jupyter-RHelp-server",
  # py_modules rather than packages, since we only have 1 file
  py_modules=['RHelp'],
  entry_points={
      'jupyter_serverproxy_servers': [
          # name = packagename:function_name
          'RHelp = RHelp:setup_RHelp',
      ]
  },
  install_requires=['jupyter-server-proxy'],
)
