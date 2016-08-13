<div class="tabs-head"
     data-tabs=idea{{idea['id']}}>
    <input type=radio name=tab-idea{{idea['id']}}
           data-tabs=idea{{idea['id']}}
           value=view checked />
    <input type=radio name=tab-idea{{idea['id']}}
           data-tabs=idea{{idea['id']}}
           value=edit />
</div>

<div class="tabpane"
     data-tabs=idea{{idea['id']}}
     data-tabpane=view>
    <pre>{{idea['text']}}</pre>
    <p>
        %if idea['sorted'] is None:
        Unsorted
        %elif idea['crankfile']:
        sorted to: Crankfile
        %else:
        sorted to: {{idea['project_name']}}
        %end
    </p>
    <!--TODO date of creation-->
    <a class='tab-selector'
       data-tabs=idea{{idea['id']}}
       data-tabpane=edit>edit
    </a>
</div>

<div class="tabpane"
     data-tabs=idea{{idea['id']}}
     data-tabpane=edit>
    <a class='tab-selector'
       data-tabs=idea{{idea['id']}}
       data-tabpane=view>back
    </a><br/>
    <form method=POST action={{app.get_url('ed_idea', id=idea['id'])}}>
        <textarea required name=text
                  rows=5 cols=60 style="resize: both;"
                  placeholder="new action">{{idea['text']}}</textarea>
        </br>
        Sort to: <select name=project>
            %if idea['sorted'] is None:
            <option value='' selected>Select Project</option>
            %end
            <option value='crankfile'{{ ' selected' if idea['crankfile'] else '' }}>Crankfile</option>
            %for project in sort_to:
            <option value={{project['id']}}{{ ' selected' if idea['project_id'] == project['id'] else ''}}>
                {{project['name']}}
                %if idea['project_id'] == project['id']:
                (no change)
                %end
            </option>
            %end
        </select>
        </br>
        <button type=submit>Edit</button>
    </form>

    <div style="border: thin red solid;">
        <h6 class='collapse-head collapse-selector'
            data-collapse=del-idea{{idea['id']}}>
            <input type=checkbox>Danger Zone
        </h6>
        <form method=POST action={{app.get_url('ed_idea', id=idea['id'])}}
              class='collapse'
              data-collapse=del-idea{{idea['id']}}>
            <select name=delete>
                <option value='no' selected></option>
                <option value='yes'>Delete</option>
            </select>
            <button type=submit>Confirm</button>
        </form>
    </div>
</div>
