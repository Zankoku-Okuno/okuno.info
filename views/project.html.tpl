% rebase('base.html.tpl')
<h2>Project</h2>

<h3>{{project['name']}}</h3>
<p>{{project['description']}}</p>

%if ideas:
<h3>Ideas</h3>
<ul>
    %for idea in ideas:
    <li>
        <pre>{{idea['text']}}</pre>
    </li>
    %end
</ul>
%end

<h3>Actions</h3>
<form method=POST action="{{app.get_url('mk_action')}}">
    <input type=hidden name=project_id value={{project['id']}} />
    <textarea required name=text
              rows=5 cols=60 style="resize: both;"
              placeholder="new action"></textarea></br>
    <button type=submit>Create</button>
</form>

<ol>
    %for action in actions:
    <li>
        <!--<pre>{{action['text']}}</pre>-->
        %if action['completed'] is None:
        <form method=POST action="{{app.get_url('ed_action', id=action['id'])}}">
            <button type=submit name='completed' value=1>☐</button> {{action['text']}}
        </form>
        %else:
        <form method=GET action="{{app.get_url('action', id=action['id'])}}">
            <button type=submit>☑</button> {{action['text']}}
        </form>
        %end
    </li>
    %end
</ol>
