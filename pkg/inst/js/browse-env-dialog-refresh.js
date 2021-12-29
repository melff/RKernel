require([
    'jquery',
    'base/js/namespace',
    'base/js/events',
    ],function($,Jupyter,events){
      function handle_output(msg){
        console.log(msg);
        var html_data = msg.content.data['text/html'];
        $('#ui-dialog-env-browser-table').html(html_data)
      }
    
      var callbacks = {
        iopub : {
             output : handle_output,
        }
      }
    
      function env_browser_table_refresh() {
        Jupyter.notebook.kernel.execute('RKernel:::env_browser_table()',callbacks)
      } 
      
      events.on('execute.CodeCell', env_browser_table_refresh);
});
