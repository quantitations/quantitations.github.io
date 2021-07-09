function ResetConfirm() {
  response = confirm('Are you sure you want to reset your progress?');
  if (response) {
    score = 0;
    $.cookie('score', score);
    level = 0;
    UpdateProgress();
    UpdateLevel();
    GetNextQuestion();
    $.ajax({ type: 'DELETE', url: '/Vocabulary/Quiz-Games', data: { id: userid } });
  }
  else {
    $('#reset').blur();
  }
}

function randomChoice(end) {
    return Math.floor(Math.random()*end);
}

function randomSample(total, num) {
  var choices = new Array(5);
  for (var i=0; i<num; i++) {
    var unique = false;
    while(!unique) {
      thischoice = randomChoice(total-i);
      unique = true;
      for(j=0; j<i; j++) {
        if(thischoice == choices[j]) unique = false;
      }
    }
    choices[i] = thischoice;
  }
  return choices;
}

function PickFive(level) {
  var part = [a, n, v][randomChoice(3)];
  var start = level*20;
  var sample = randomSample(60,5);
  var fivewords = new Array(5);
  for (var i=0; i<5; i++) {
    fivewords[i] = part[start+sample[i]];
  }
  return fivewords;
}

function GenerateWords() {
  if (window.level >= 5) {
    return PickFive(randomChoice(5));
  }
  return PickFive(window.level);
}

var correctIndex;
var needreload = false;
//var explain = $('#explainpurchase');

//if (explain) {
//	if ($.cookie('insertcoinPlay all five levels')) { explain.text('Thanks for your support!'); }
//	else { $.get('/Ping'); }
//}

function GetNextQuestion() {
  if (needreload) { 
	  if (userid == '0') { window.location = "/Vocabulary/Quiz-Games?purchase=t"; }
	  else { location.reload(); }
  }
  $('.hide').addClass("hidden");
  $('#choicestable tr').css('background-color', '#ffffff');
  $('.correct').removeClass('correctrow').removeClass('correct');
  $('#choicestable button').attr('disabled', false);
  $('.gray').addClass('black').removeClass('gray');

  var fivewords = GenerateWords();
  window.correctIndex = randomChoice(5);

  $('.question h3').html(fivewords[window.correctIndex][1]);
  for (var i=0; i<5; i++) {
    $('tr.'+i+' b').html(fivewords[i][0]);
    $('tr.'+i+' span').html(fivewords[i][1]);
  }
  $('tr.'+window.correctIndex).addClass('correct');
}

function UpdateScore(iscorrect) {
  //if (explain) {
	//  if (explain.text() != 'Thanks for your support!') { explain.text('Thanks for your support!'); }
	//  else { explain.remove(); explain = false; }
  //}
  if (iscorrect==1) {
    var togo = 100-score%100;
    if (togo > 3) {
      score += 3;
    }
    else {
      score += togo;
      level += 1;
      UpdateLevel();
    }
  }
  else {
    if (score%100 > 3) {
      score -= 4;
    }
    else {
      score -= score%100;
    }
  }
  $.cookie('score', score);
  UpdateProgress();
}

function UpdateProgress() {
  $('#progressbar').css('width',score%100+'%');
}

function UpdateLevel() {
  var imagesource = level+1;
  if (imagesource>6) imagesource = 6;
  $('#levelimg').attr('src',"/images/"+imagesource+".png");
  if (userid != 0) {
    $.ajax({ type: 'POST', url: '/Vocabulary/Quiz-Games', data: { score: score, id: userid } })
  	.done(function() {		
  	    if (level == 2) { needreload = true; }
  	});
  }
  else if (level == 2) { needreload = true; }
}

function Submit(object) {
  $('#choicestable button').blur();
  if ($(object).parent().parent().hasClass(correctIndex+'')) {
    $('#grade').html('Correct').css('color', '#009ad5');
  }
  else {
    $(object).parent().parent().css('background-color', '#ffe0cc');
    $('#grade').html('Incorrect').css('color', '#f60');
  }
  $('#choicestable button').attr('disabled', true);
  $('.hide').removeClass("hidden");
  $('.correct').addClass('correctrow').css('background-color', '#d5f3ff');
  $('.black').addClass('gray').removeClass('black');
  UpdateScore(+($(object).parent().parent().hasClass(correctIndex+'')));
  return(false);
}

$(document).ready( GetNextQuestion() );
