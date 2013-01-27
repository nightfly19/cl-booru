$(document).ready(function(){
    $("#search-submit").click(function(e){
      });

    $("#search-submit").attr("href", "/posts/search/tags/"+$("#query").val());

    $("#query").keyup(function(e){
      $("#search-submit").attr("href", "/posts/search/tags/"+$("#query").val());
      });

    $("#tag-search-form").submit(function(e){
      window.location = $("#search-submit").attr("href");
      return false;
      });

    $("#post-edit-button").click(function(e){
      e.preventDefault();
      $("#post-edit-form").slideToggle();
      $("#post-comment-form").slideUp();
      });

    $(".action-buttons").show();

    $("#post-comment-button").click(function(e){
        e.preventDefault();
        $("#post-comment-form").slideToggle();
        $("#post-edit-form").slideUp();
        });

    $("#post-comment-form h3").hide();
    $("#post-edit-form h3").hide();
    var imageNumber = $("#post-number").attr("value");

    var voteOnPost = function(image, vote){
      var url = "/api/vote-on-post?image="+imageNumber+"&vote="+vote;
      $.getJSON(url, function(data){
          if(parseInt(data) != NaN){
            $("#image-score").text(String(data));
          }
      });
    };

    $("#image-vote-up").click(function(e){
        e.preventDefault();
        voteOnPost(imageNumber,"up");
        });
    $("#image-vote-down").click(function(e){
        e.preventDefault();
        voteOnPost(imageNumber,"down");
        });
});
