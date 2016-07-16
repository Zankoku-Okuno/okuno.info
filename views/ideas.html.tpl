<form method=POST action="ideas">
    <textarea name=text required
              rows=5 cols=60 style="resize: both;"
              placeholder="new idea"></textarea><br/>
    <button type=submit>Create</button>
</form>
<ul>
    %for idea in ideas:
    <li class='.idea'>
        <p>{{idea['text']}}</p>
        <form method=POST action="ideas/sort">
            <input type=hidden name=idea value={{idea['id']}} />
            <select name=project required>
                %if idea['project_id'] is None and not idea['crankfile']:
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
