% rebase('base.html.tpl')
<h2>Action</h2>
<h3>{{action['project_name']}}</h3>
<form method=POST action={{app.get_url('ed_action', id=action['id'])}}>
    <textarea name=text
              rows=5 cols=60 style="resize: both;">{{action['text']}}</textarea>
    <br/>
    <button type=submit>Edit</button>
    %if action['completed'] is None:
    <button type=submit name=completed value=1>☐ | Complete</button>
    %else:
    <button type=submit name=completed value=0>☑ | Reopen</button>
    %end
</form>
<form method=POST action={{app.get_url('ed_action', id=action['id'])}}
      style="border: thin solid red; margin-top: 5em; padding: 0.5em;">
    <select name=delete>
        <option value='no' selected></option>
        <option value='yes'>Delete</option>
    </select>
    <button type=submit>Confirm</button>
</form>
<!-- TODO show project name -->
<!-- TODO edit text-->
