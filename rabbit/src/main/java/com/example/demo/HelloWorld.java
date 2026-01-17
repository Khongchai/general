package com.example.demo;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


@Service
class Sender {
    void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        try (Connection connection = factory.newConnection();
             Channel channel = connection.createChannel()) {

            // idempotent. If not exist, created
            channel.queueDeclare("hello_queue", false, false, false, null);

            String message = "Hello World!";
            channel.basicPublish("", "hello_queue", null, message.getBytes());

            System.out.println(" [x] Sent '" + message + "'");
        }
    }
}

@Service
class Receiver {
    void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection(); Channel channel = connection.createChannel();
        channel.queueDeclare("hello_queue", false, false, false, null);
        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            String message = new String(delivery.getBody(), "UTF-8");
            System.out.println(" [x] Received '" + message + "'");
        };
        channel.basicConsume("hello_queue", true, deliverCallback, consumerTag -> { });
    }
}

@Service
public class HelloWorld {
    @Autowired
    private Sender sender;
    @Autowired
    private Receiver receiver;

    public void act() throws Exception {
        // Comment either one out to test sending without receiver or receiving tasks stored in a queue.
        receiver.act();
        sender.act();
    }
}
