
var getGet_eventByEventByDistByDays = function(event, dist, days, onSuccess, onError)
{
  $.ajax(
    { url: '/get_event/' + encodeURIComponent(event) + '/' + encodeURIComponent(dist) + '/' + encodeURIComponent(days) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getPredictByEventByDist = function(event, dist, onSuccess, onError)
{
  $.ajax(
    { url: '/predict/' + encodeURIComponent(event) + '/' + encodeURIComponent(dist) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getPredictStepsByEventByDist = function(event, dist, onSuccess, onError)
{
  $.ajax(
    { url: '/predictSteps/' + encodeURIComponent(event) + '/' + encodeURIComponent(dist) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getMapByEventByDist = function(event, dist, onSuccess, onError)
{
  $.ajax(
    { url: '/map/' + encodeURIComponent(event) + '/' + encodeURIComponent(dist) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
