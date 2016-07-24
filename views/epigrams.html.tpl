% rebase('base.html.tpl')
<h2>Epigrams</h2>

<form method=POST action="{{app.get_url('mk_epigram')}}">
    <textarea name=text required
              rows=5 cols=60 style="resize: both;"
              placeholder="new epigram"></textarea><br/>
    <input type=text name=credit
           placeholder="accreditation" /><br/>
    <button type=submit>Create</button>
</form>
<ul>
    %for epigram in epigrams:
    <li class='.epigram'>
        <p>{{epigram['text']}}</p>
        %if epigram['credit']:
        <p><small>– {{epigram['credit']}}</small></p>
        %end
    </li>
    %end
</ul>
