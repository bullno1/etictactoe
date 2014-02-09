$(main);

function main() {
	"use strict";

	$("#create-game").click(function() {
		$.ajax("/games", {
			method: "POST",
			statusCode: {
				201: function(output, status, xhr) {
					window.location = xhr.getResponseHeader("location");
				}
			},
			error: function(err) {
				console.log(err);
			}
		});
	});
}
