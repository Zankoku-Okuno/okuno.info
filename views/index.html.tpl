% rebase('base.html.tpl')

<div>
    <form method=GET action={{app.get_url('login')}} style="display: inline;"><button>Login</button></form>
    <form method=POST action={{app.get_url('logout')}} style="display: inline;"><button>Logout</button></form>
</div>

<h2>Home</h2>

<ul>
    <li><a href={{app.get_url('projects')}}>Projects</a></li>
    <li><a href={{app.get_url('ideas')}}>Ideas</a></li>
    <li><a href={{app.get_url('techs')}}>Technologies</a></li>
    <li><a href={{app.get_url('epigrams')}}>Epigrams</a></li>
</ul>
