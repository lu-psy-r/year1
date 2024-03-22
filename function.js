$(document).ready(function () {

var start = new Date();
var end = new Date();
var time = new Date().getTime();

   //Set wk1 hours
if (time > start.setHours(00,01) && time < end.setHours(11,59)) {
    $('.open_wk1').show();
    $('.closed_wk1').hide();
}
else {
    $('.open_wk1').hide();
    $('.closed_wk1').show();
    }
    
    //Set wk2hours
if (time > start.setHours(12,00) && time < end.setHours(23,59)) {
    $('.open_wk2').show();
    $('.closed_wk2').hide();
}
else {
    $('.open_wk2').hide();
    $('.closed_wk2').show();
    }
});