% rebase('base.html.tpl')
<h2>Crankfile</h2>

<p><a href={{app.get_url('ideas')}}>all ideas</a></p>

<ul class='ideas'>
    %for idea in ideas:
    <li class='idea'>
    	%include("idea.frag.html", idea=idea, sort_to=sort_to)
    </li>
    %end
</ul>
