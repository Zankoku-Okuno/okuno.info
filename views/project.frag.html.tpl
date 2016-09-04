<h3 class="collapse-head collapse-selector"
    data-collapse=project{{project['id']}}>
        <input type=checkbox checked />
        {{project['name']}}
</h3>
<div class="collapse tabs-head"
     data-collapse=project{{project['id']}}
     data-tabs=project{{project['id']}}>
    <input type=radio name=tab-project{{project['id']}}
           data-tabs=project{{project['id']}}
           value=view checked />
    <input type=radio name=tab-project{{project['id']}}
           data-tabs=project{{project['id']}}
           value=edit />
</div>

<pre class="collapse tabpane"
   data-collapse=project{{project['id']}}
   data-tabs=project{{project['id']}}
   data-tabpane=view>
    {{project['description']}}
</pre>

<div class='collapse tabpane'
      data-collapse=project{{project['id']}}
      data-tabs=project{{project['id']}}
      data-tabpane=edit>
    <a class='tab-selector'
       data-tabs=project{{project['id']}}
       data-tabpane=view>back
    </a>
</div>
<div class='collapse tabpane'
      data-collapse=project{{project['id']}}
      data-tabs=project{{project['id']}}
      data-tabpane=view>
    <a class='tab-selector'
       data-tabs=project{{project['id']}}
       data-tabpane=edit>edit
    </a>
</div>

<form method=POST action="{{app.get_url('ed_project', id=project['id'])}}"
      class="collapse tabpane"
      data-collapse=project{{project['id']}}
      data-tabs=project{{project['id']}}
      data-tabpane=edit>
    <input type=text name=name value="{{project['name']}}" />
    <br/>
    <textarea name=description
              rows=5 cols=60 style="resize: both;">{{project.description}}</textarea>
    <br/>
    <button type=submit>Edit</button>
</form>