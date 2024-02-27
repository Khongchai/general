// @ts-check

function main() {
    // create a tcp listener
    const net = require('node:net');
    // add a listener to port "127.0.0.1:9999"
     
    const server = net.createServer((socket) => {
        socket.setEncoding("utf-8");
        socket.on("data", (data) => {
            console.log(data);
            socket.write('HTTP/1.1 200 OK\r\n\r\nHello World', 'utf8', () => {
                socket.end();
            });
        });
    });

    server.listen(9999, '127.0.0.1', () => {
        console.log('server bound');
    });
}

main()
