%rebase('base.html.tpl')
<h2>Project</h2>

<div class='project_desc'>
    %include('project.frag.html', project=project)
</div>

%if ideas:
<h3>Ideas</h3>
<ul class='ideas'>
    %for idea in ideas:
    <li class='idea'>
        %include("idea.frag.html", idea=idea)
    </li>
    %end
    <li class='idea'>
        <div class='collapse-head collapse-selector'
             data-collapse=new-idea>
            <input type=checkbox>New Idea
        </div>
        <form method=POST action="{{app.get_url('mk_idea')}}"
              class='collapse'
              data-collapse=new-idea>
            <input type=hidden name=project_id value={{project['id']}} />
            <textarea name=text required
                      rows=5 cols=60 style="resize: both;"
                      placeholder="new idea"></textarea><br/>
            <button type=submit>Create</button>
        </form>
    </li>
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

<ol class='actions'>
    %for action in actions:
    <li class='action tabs-head'
        data-tabs=action{{action['id']}}>
        <input type=radio name=tab-action{{action['id']}}
               data-tabs=action{{action['id']}}
               value=view checked />
        <input type=radio name=tab-action{{action['id']}}
               data-tabs=action{{action['id']}}
               value=edit />

        <form method=POST action={{app.get_url('ed_action', id=action['id'])}}
              class="tabpane"
              data-tabs=action{{action['id']}}
              data-tabpane=view>
            %if action['completed'] is None:
            <button type=submit name=completed value=1>☐</button>
            %else:
            ☑
            %end
            <pre style="display: inline;">{{action['text']}}</pre>
            <small class='tab-selector'
               data-tabs=action{{action['id']}}
               data-tabpane=edit>edit
            </small>
        </form>

        <div class="tabpane"
             data-tabs=action{{action['id']}}
             data-tabpane=edit>
            <form method=POST action={{app.get_url('ed_action', id=action['id'])}}>
                %if action['completed'] is None:
                <button type=submit name=completed value=1>☐ | Complete</button>
                %else:
                <button type=submit name=completed value=0>☑ | Reopen</button>
                %end
                <pre style="display: inline;">{{action['text']}}</pre>
                <small class='tab-selector'
                   data-tabs=action{{action['id']}}
                   data-tabpane=view>back
                </small>
            </form>

            <form method=POST action={{app.get_url('ed_action', id=action['id'])}}>
                <textarea name=text
                          rows=5 cols=60 style="resize: both;">{{action['text']}}</textarea>
                <br/>
                <button type=submit>Edit</button>
            </form>

            <div style="border: thin red solid;"
                 class="tabpane"
                 data-tabs=action{{action['id']}}
                 data-tabpane=edit>
                <h6 class='collapse-head collapse-selector'
                    data-collapse=del-action{{action['id']}}>
                    <input type=checkbox>Danger Zone
                </h6>
                <form method=POST action={{app.get_url('ed_action', id=action['id'])}}
                      class='collapse'
                      data-collapse=del-action{{action['id']}}>
                    <select name=delete>
                        <option value='no' selected></option>
                        <option value='yes'>Delete</option>
                    </select>
                    <button type=submit>Confirm</button>
                </form>
            </div>
        </div>
        
    </li>
    %end
</ol>


