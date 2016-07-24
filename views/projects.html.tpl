% rebase('base.html.tpl')
<h2>Projects</h2>

<form method=POST action="{{app.get_url('mk_project')}}">
    <input type=text required name=name
           placeholder="new project" /></br>
    <textarea name=description
              rows=5 cols=60 style="resize: both;"
              placeholder="description"></textarea></br>
    <button type=submit>Create</button>
</form>
<ul>
    %for project in projects:
    <li class='.project'>
        <h3>
            <a href={{app.get_url('project', id=project['id'])}}>{{project['name']}}</a>
            <small>({{project['actions_count']}})</small>
        </h3>
        <p>{{project['description']}}</p>
    </li>
    %end
</ul>
