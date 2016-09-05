<h3 class="collapse-head collapse-selector"
    data-collapse=project{{project.id}}>
        <input type=checkbox checked />
        <a href={{app.get_url('project', id=project.id)}}>{{project.name}}</a>
        %if hasattr(project, 'actions_count'):
        ({{project.actions_count}})
        %end
</h3>
<div class="collapse tabs-head"
     data-collapse=project{{project.id}}
     data-tabs=project{{project.id}}>
    <input type=radio name=tab-project{{project.id}}
           data-tabs=project{{project.id}}
           value=view checked />
    <input type=radio name=tab-project{{project.id}}
           data-tabs=project{{project.id}}
           value=edit />
</div>

<div class="collapse tabpane"
   data-collapse=project{{project.id}}
   data-tabs=project{{project.id}}
   data-tabpane=view>
    <div class="md">{{project.description}}</div>
    <small class='tab-selector'
       data-tabs=project{{project.id}}
       data-tabpane=edit>edit
    </small>
 </div>

<form method=POST action="{{app.get_url('ed_project', id=project.id)}}"
      class="collapse tabpane"
      data-collapse=project{{project.id}}
      data-tabs=project{{project.id}}
      data-tabpane=edit>
    <input type=text name=name value="{{project.name}}" />
    <br/>
    <textarea name=description
              rows=5 cols=60 style="resize: both;">{{project.description}}</textarea>
    <br/>
    <button type=submit>Edit</button>
    <small class='tab-selector'
       data-tabs=project{{project.id}}
       data-tabpane=view>back
    </small>
</form>