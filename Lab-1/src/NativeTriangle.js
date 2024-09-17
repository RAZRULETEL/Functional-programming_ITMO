const { readFile } = require('node:fs/promises');



readFile("src/triangle.txt", { encoding: 'utf8' })
    .then(data => {
        const arrays = data
            .split('\n')
            .map(line => line.split(" ").map(e => +e))
            .reverse();
        for(let i = 1; i < arrays.length; i++) {
            for(let j = 0; j < arrays[i].length; j++) {
                arrays[i][j] += Math.max(arrays[i - 1][j], arrays[i - 1][j + 1]);
            }
        }
        console.log(arrays.reverse()[0][0]);
    })
