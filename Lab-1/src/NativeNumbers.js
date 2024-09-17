const bigInt = require("big-integer");
const { readFile } = require('node:fs/promises');

const bigZero = bigInt();

 readFile("src/numbers.txt", { encoding: 'utf8' })
    .then(data => {
        const bigArr = data
            .split('\n')
            .map(line => bigInt(line));
        console.log(bigArr.reduce((a, b) => a.add(b), bigZero).toString());
    })
