require.config({
    paths: {
        jquery: 'https://code.jquery.com/jquery-3.5.1',
        datatables: 'https://cdn.datatables.net/1.11.1/js/jquery.dataTables.min',
    }
});

$('head').append('<link rel="stylesheet" type="text/css" \
                href = "https://cdn.datatables.net/1.11.1/css/jquery.dataTables.min.css" > ');

$('head').append('<style> table td { text-overflow: ellipsis; overflow: hidden; } </style>');


$('head').append('<style> \
                 .rendered_html thead tr, .rendered_html thead th \
                 { text-align: centre; } \
                 </style>');

$('head').append('<style> \
                 .rendered_html .tab-left-aligned tr, \
                 .rendered_html .tab-left-aligned th, \
                 .rendered_html .tab-left-aligned td \
                 { text-align: left; } \
                 </style>');

$('head').append('<style> \
                 .rendered_html table code \
                 { background-color: unset; } \
                 </style>');

$('head').append('<style> \
                 .dataTables_scrollBody thead \
                 { visibility: collapse; } \
                 </style>');

$('head').append('<style> \
                 .rendered_html .dataTable tbody tr:nth-child(2n+1) \
                 { background-color: unset; } \
                 </style>');

// alert('datatables_connected ...');
 
