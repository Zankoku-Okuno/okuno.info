%rebase('base.html.tpl')
<h2>Project</h2>

<div style="border: thin black solid; border-radius: 5px;">
    <h3 class="collapse-head collapse-selector"
        data-collapses=project{{project['id']}}>
            <input type=checkbox checked />
            {{project['name']}}
    </h3>
    <div class="collapse tab-head"
         data-collapses=project{{project['id']}}
         data-tabs=project{{project['id']}}>
        <input type=radio name=tab-project{{project['id']}}
               data-tabs=project{{project['id']}}
               value=view checked />
        <input type=radio name=tab-project{{project['id']}}
               data-tabs=project{{project['id']}}
               value=edit />
    </div>

    <p class="collapse tab"
       data-collapses=project{{project['id']}}
       data-tabs=project{{project['id']}}
       data-tabpane=view>
        {{project['description']}}
    </p>

    <div class='collapse tab'
          data-collapses=project{{project['id']}}
          data-tabs=project{{project['id']}}
          data-tabpane=edit>
        <a class='tab-selector'
           data-tabs=project{{project['id']}}
           data-tabpane=view>View
        </a>
    </div>
    <div class='collapse tab'
          data-collapses=project{{project['id']}}
          data-tabs=project{{project['id']}}
          data-tabpane=view>
        <a class='tab-selector'
           data-tabs=project{{project['id']}}
           data-tabpane=edit>Edit
        </a>
    </div>

    <form method=POST action="{{app.get_url('ed_project', id=project['id'])}}"
          class="collapse tab"
          data-collapses=project{{project['id']}}
          data-tabs=project{{project['id']}}
          data-tabpane=edit>
        <input type=text name=name value={{project['name']}} />
        <br/>
        <textarea name=description
                  rows=5 cols=60 style="resize: both;">{{project['description']}}</textarea>
        <br/>
        <button type=submit>Edit</button>
    </form>
    
</div>

%if ideas:
<h3>Ideas</h3>
<ul>
    %for idea in ideas:
    <li>
        <pre>{{idea['text']}}
  <small><a href={{app.get_url('idea', id=idea['id'])}} >edit</a></small></pre>
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
            <button type=submit name='completed' value=1>☐</button> {{action['text']}} <small><a href={{app.get_url('action', id=action['id'])}}>edit</a></small>
        </form>
        %else:
        <button>☑</button> {{action['text']}} <small><a href={{app.get_url('action', id=action['id'])}}>edit</a></small>
        %end
    </li>
    %end
</ol>


