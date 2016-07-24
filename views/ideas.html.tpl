% rebase('base.html.tpl')
<h2>Ideas</h2>

<form method=POST action="{{app.get_url('mk_idea')}}">
    <textarea name=text required
              rows=5 cols=60 style="resize: both;"
              placeholder="new idea"></textarea><br/>
    <button type=submit>Create</button>
</form>
<ul>
    %for idea in ideas:
    <li class='.idea'>
        <p>{{idea['text']}}
        <small><a href={{app.get_url('idea', id=idea['id'])}}>edit</a></small></p>
        <form method=POST action="{{app.get_url('ed_idea', id=idea['id'])}}">
            <select name=project required>
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
            <button type=submit>Sort</button>
        </form>
        <!--TODO project it is sorted into-->
        <!--TODO date of creation-->
    </li>
    %end
</ul>
