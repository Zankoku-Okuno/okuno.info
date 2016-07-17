<h3>{{action['project_name']}}</h3>
<pre>{{action['text']}}</pre>
<form method=POST action={{app.get_url('ed_action', id=action['id'])}}>
    %if action['completed'] is None:
    <button type=submit name=completed value=1>☐ | Complete</button>
    %else:
    <button type=submit name=completed value=0>☑ | Reopen</button>
    %end
</form>
<!-- TODO show project name -->
<!-- TODO edit text-->
