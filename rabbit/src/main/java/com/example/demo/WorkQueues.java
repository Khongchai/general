package com.example.demo;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;


@Service
class WorkQueuesSender {
    void act(String message) throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        try (Connection connection = factory.newConnection();
             Channel channel = connection.createChannel()) {

            // idempotent. If not exist, created
            channel.queueDeclare("hello_queue", false, false, false, null);

            channel.basicPublish("", "hello_queue", null, message.getBytes());

            System.out.println(" [x] Sent '" + message + "'");
        }
    }
}

@Service
class WorkQueuesReceiver {
    void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection(); Channel channel = connection.createChannel();
        channel.queueDeclare("hello_queue", false, false, false, null);
        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            final String message = new String(delivery.getBody(), StandardCharsets.UTF_8);
            System.out.println(" [x] Received '" + message + "'");
            try {
                doWork(message);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        };
        channel.basicConsume("hello_queue", true, deliverCallback, consumerTag -> { });
    }

    private static void doWork(String task) throws InterruptedException {
        for (char ch: task.toCharArray()) {
            if (ch == '.') Thread.sleep(1000);
        }
    }
}

@Service
public class WorkQueues {
    @Autowired
    private WorkQueuesSender sender;
    @Autowired
    private WorkQueuesReceiver receiver;

    public void act() throws Exception {
        receiver.act();
        sender.act("....");
    }
}
