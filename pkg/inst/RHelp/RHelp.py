def setup_RHelp():
    return {
        'command': ['R', '-q', 
                    '-e','options(help.ports={port}); tools::startDynamicHelp(); repeat Sys.sleep(60)'],
        'absolute_url': False
    }
