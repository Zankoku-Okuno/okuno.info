<select name=project>
  if idea.sorted is None:
  <option value=''{{ ' selected' if idea.sorted is None else '' }}>
    Unsorted
    %if idea.sorted is None:
    (no change)
    %end
  </option>
  <option value='crankfile'{{ ' selected' if idea.crankfile else '' }}>
    Crankfile
    %if idea.crankfile:
    (no change)
    %end
  </option>
  %for project in sort_to:
  <option value={{project.id}}{{ ' selected' if idea.project_id == project.id else ''}}>
    {{project.name}}
    %if idea.project_id == project.id:
    (no change)
    %end
  </option>
  %end
</select>