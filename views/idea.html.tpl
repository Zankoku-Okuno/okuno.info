% rebase('base.html.tpl')
<h2>Idea</h2>

<form method=POST action={{app.get_url('ed_idea', id=idea['id'])}}>
    <textarea required name=text
              rows=5 cols=60 style="resize: both;"
              placeholder="new action">{{idea['text']}}</textarea>
    </br>
    Sort: <select name=project>
        %if idea['sorted'] is None:
        <option value='' selected>Select Project</option>
        %end
        <option value='crankfile'{{ ' selected' if idea['crankfile'] else '' }}>Crankfile</option>
        %for project in projects:
        <option value={{project['id']}}{{ ' selected' if idea['project_id'] == project['id'] else ''}}>
            {{project['name']}}
        </option>
        %end
    </select>
    </br>
    <button type=submit>Edit</button>
</form>
<form method=POST action={{app.get_url('ed_idea', id=idea['id'])}}
      style="border: thin solid red; margin-top: 5em; padding: 0.5em;">
    <select name=delete>
        <option value='no' selected></option>
        <option value='yes'>Delete</option>
    </select>
    <button type=submit>Confirm</button>
</form>
