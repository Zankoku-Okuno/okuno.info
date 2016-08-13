% rebase('base.html.tpl')
<h2>Projects</h2>


<ul class='projects'>
    %for project in projects:
    <li class='project'>
        <h3>
            <a href={{app.get_url('project', id=project['id'])}}>{{project['name']}}</a>
            <small>({{project['actions_count']}})</small>
        </h3>
        <div class='tabs-head'
             data-tabs=project{{project['id']}}>
            <input type=radio name=tab-project{{project['id']}} checked
                   data-tabs=project{{project['id']}}
                   value=view>
            <input type=radio name=tab-project{{project['id']}}
                   data-tabs=project{{project['id']}}
                   value=edit>
        </div>

        <div class='tabpane'
             data-tabs=project{{project['id']}}
             data-tabpane=view>
            <pre>{{project['description']}}</pre>
            <small class='tab-selector' data-tabs=project{{project['id']}} data-tabpane=edit>edit</small>
        </div>

        <form method=POST action="{{app.get_url('ed_project', id=project['id'])}}"
              class="tabpane"
              data-tabs=project{{project['id']}}
              data-tabpane=edit>
            <small class='tab-selector'
                   data-tabs=project{{project['id']}}
                   data-tabpane=view>back</small><br/>
            <input type=text name=name value={{project['name']}} />
            <br/>
            <textarea name=description
                      rows=5 cols=60 style="resize: both;">{{project['description']}}</textarea>
            <br/>
            <button type=submit>Edit</button>
        </form>
    </li>
    %end
    
    <li class='project'>
        <div class="collapse-head collapse-selector"
             data-collapse=new-project>
            <input type=checkbox>New Project
        </div>
        <form method=POST action="{{app.get_url('mk_project')}}"
              class='collapse'
              data-collapse=new-project>
            <input type=text required name=name
                   placeholder="new project" /></br>
            <textarea name=description
                      rows=5 cols=60 style="resize: both;"
                      placeholder="description"></textarea></br>
            <button type=submit>Create</button>
        </form>
    </li>
</ul>
