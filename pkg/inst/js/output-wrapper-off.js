require([
    'jquery',
    'base/js/namespace',
],function($,Jupyter){

    if (Jupyter.notebook.metadata.outputWrapper === undefined) {
        Jupyter.notebook.metadata.outputWrapper = {};
    }
    
    Jupyter.notebook.metadata.outputWrapper.position = {
        'left': $('#output-wrapper').css('left'),
        'top': $('#output-wrapper').css('top'),
        'width': $('#output-wrapper').css('width'),
        'height': $('#output-wrapper').css('height'),
        'right': $('#output-wrapper').css('right')
    };

    $('#output').remove();
    $('#output-wrapper').remove();
})
