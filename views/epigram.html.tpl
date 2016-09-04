% rebase("base.html.tpl")
<h2>Epigram</h2>

<form method=POST action={{app.get_url('ed_epigram', id=epigram.id)}}>
    <textarea name=text
              rows=5 cols=60 style="resize: both;">{{epigram.text}}</textarea>
    <br/>
    <input type=text name=credit value="{{epigram.credit or ''}}"/>
    <br/>
    <button type=submit>Edit</button>
</form>
