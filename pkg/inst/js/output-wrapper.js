require([
    'jquery',
    'base/js/namespace',
],function($,Jupyter){

    var output_wrapper = $('<div id="output-wrapper"/>')
        .append($("<div/>").attr("id", "output").addClass('output'));

    output_wrapper.appendTo("body")
        .css('position','fixed')
        .css('left','calc(100% - 100px)')
        .css('top','160px')
        .draggable()
        .resizable({
            handles: 'e, s, w'
        })
        .draggable()
        .css('display','block')
        .css('background-color','#fff')
        .css('z-index','250')
        .css('opacity','1');

    if (Jupyter.notebook.metadata.outputWrapper !== undefined) {
        
        if (Jupyter.notebook.metadata.outputWrapper.position !== undefined) {
            $('#output-wrapper').css(Jupyter.notebook.metadata.outputWrapper.position);
        }
    }
});
