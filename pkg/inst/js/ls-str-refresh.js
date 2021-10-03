require([
    'jquery',
    'base/js/namespace',
    'base/js/events',
    ],function($,Jupyter,events){
      function handle_output(msg){
        console.log(msg);
        var html_data = msg.content.data['text/html'];
        $('#output').html(html_data)
      }
    
      var callbacks = {
        iopub : {
             output : handle_output,
        }
      }
    
      function ls_str_refresh() {
        Jupyter.notebook.kernel.execute('ls_str()',callbacks)
      } 
      
      events.on('execute.CodeCell', ls_str_refresh);
});
