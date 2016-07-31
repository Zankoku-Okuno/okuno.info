% rebase('base.html.tpl')
<h2>Crankfile</h2>

<p><a href={{app.get_url('ideas')}}>all ideas</a></p>

<ul>
    %for idea in ideas:
    <li class='.idea'>
        <p>{{idea['text']}}
        <small><a href={{app.get_url('idea', id=idea['id'])}}>edit</a></small></p>
        <!--TODO date of creation-->
    </li>
    %end
</ul>
