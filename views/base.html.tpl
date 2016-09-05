% if request.headers.get('X-PJAX') is None:
<!DOCTYPE html>
<html>
    <head>
        <meta charset="utf-8" />
        <title>Okuno</title>
        <script type='text/javascript' src='/static/fetch.js'></script>
        
        <link rel=stylesheet href='/static/shim.css' />
        <script type='text/javascript' src='/static/shim.js'></script>

        <script type='text/javascript' src='/static/markdown-js/markdown.min.js'></script>
        <script type='text/javascript' src='/static/markdown.js'></script>

        <link rel=stylesheet href='/static/collapse.css' />
        <script type='text/javascript' src='/static/collapse.js'></script>
        <link rel=stylesheet href='/static/tabs.css' />
        <script type='text/javascript' src='/static/tabs.js'></script>

        <link rel=stylesheet href='/static/style.css' />
    </head>
    <body>
        <h1><a href={{app.get_url('index')}}>Okuno.info</a></h1>
        {{!base}}
        <script type="text/javascript">
            init_markdown(document)
            init_collapse(document)
            init_tabs(document)
        </script>
    </body>
</html>
% else:
{{!base}}
%end