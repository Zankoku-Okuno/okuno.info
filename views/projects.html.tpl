<form method=POST action="">
    <input type=text required name=name
           placeholder="new project" /></br>
    <textarea required name=description
              rows=5 cols=60 style="resize: both;"
              placeholder="description"></textarea></br>
    <button type=submit>Create</button>
</form>
<ul>
    %for project in projects:
    <li class='.project'>
        <h3>{{project['name']}}</h3>
        <p>{{project['description']}}</p>
    </li>
    %end
</ul>
