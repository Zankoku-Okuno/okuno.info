% rebase('base.html.tpl')
<h2>Home</h2>

<ul>
    <li><a href={{app.get_url('projects')}}>Projects</a></li>
    <li><a href={{app.get_url('ideas')}}>Ideas</a></li>
    <li><a href={{app.get_url('epigrams')}}>Epigrams</a></li>
</ul>
