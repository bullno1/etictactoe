$(main);

function main() {
	var board = createBoard();

	function createBoard() {
		var board = [];
		var boardElem = $("#board");
		for(var i = 0; i < 3; ++i) {
			var rowElem = $("<div/>").addClass("row");
			var row = [];
			for(var j = 0; j < 3; ++j) {
				var tile = $("<span></span>").data("x", j + 1).data("y", i + 1);
				rowElem.append(tile);
				row.push(tile);
			}
			boardElem.append(rowElem);
			board.push(row);
		}

		return board;
	}

	var lastX;
	var lastY;
	var piece;
	$("#board span").click(function(e) {
		var tile = $(this);
		var x = tile.data("x");
		var y = tile.data("y");
		lastX = x;
		lastY = y;
		send(["play", x, y]);
	});

	var status = $("#status");
	function setStatus(msg) {
		status.text(msg);
	}

	var socket = new WebSocket("ws://" + location.host + location.pathname + "/sock");
	socket.addEventListener("error", onError);
	socket.onopen = function() {
		setStatus("Joining");
		send('join');
	}

	socket.onmessage = function(e) {
		var msg = JSON.parse(e.data);
		if(msg === "continue") {
			console.log(lastX, lastY, piece);
			setTile(lastX, lastY, piece);
			setStatus("Opponent's turn");
		}
		else {
			switch(msg[0]) {
			case "ok":
				setStatus("Waiting for opponent");
				piece = msg[1];
				lastX = null;
				lastY = null;
				console.log(piece);
				break;
			case "start":
				setStatus(msg[1] ? "Your turn" : "Opponent's turn");
				break;
			case "error":
				setStatus(msg[1]);
				break;
			case "finished":
				if(lastX != null && lastY != null) {
					setTile(lastX, lastY, piece);
				}
				var outcome = msg[1];
				if(outcome == "draw")
					setStatus("draw");
				else
					setStatus(outcome[2]);
				break;
			case "move":
				var opponentPiece = msg[1];
				var x = msg[2];
				var y = msg[3];
				setTile(x, y, opponentPiece);
				var outcome = msg[4];
				setStatus("Your turn");
				break;
			}
		}
	}

	function setTile(x, y, piece) {
		board[y-1][x-1].text(piece);
	}

	function onError(err) {
		console.log(err);
	}

	function send(msg) {
		socket.send(JSON.stringify(msg));
	}
}
