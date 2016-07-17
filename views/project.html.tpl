<h3>{{project['name']}}</h3>
<p>{{project['description']}}</p>

<form method=POST action="/actions">
    <input type=hidden name=project_id value={{project['id']}} />
    <textarea required name=text
              rows=5 cols=60 style="resize: both;"
              placeholder="new action"></textarea></br>
    <button type=submit>Create</button>
</form>

<ol>
    %for action in actions:
    <li>
        <p>{{'☐' if action['completed'] is None else '☑'}} {{action['text']}}</p> 
        %if action['completed'] is None:
        <form method=POST action="/actions">
            <input type=hidden name='id' value={{action['id']}} />
            <button type=submit name='completed' value=1>Complete</button>
        </form>
        %end
    </li>
    %end
</ol>
