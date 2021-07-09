function PopulateList() {
	var wordslist = '';
	var len = a.length;
	for (var i=0; i<len; ++i) {
		wordslist += '<tr><td>' + a[i][0] + '</td><td>' + a[i][1] + '</td></tr>';
		wordslist += '<tr><td>' + n[i][0] + '</td><td>' + n[i][1] + '</td></tr>';
		wordslist += '<tr><td>' + v[i][0] + '</td><td>' + v[i][1] + '</td></tr>';
	}
    $('#wordslist').html(wordslist);
	$('h3#jsmessage').html('')
}

$(document).ready( PopulateList() );