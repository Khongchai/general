package com.example.demo;

import com.rabbitmq.client.*;
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
            channel.queueDeclare("hello_queue_durable", false, false, false, null);

            channel.basicPublish("", "hello_queue_durable",
                    MessageProperties.PERSISTENT_TEXT_PLAIN,  // survives restart
                    message.getBytes());

            System.out.println(" [x] Sent '" + message + "'");
        }
    }
}

class WorkQueuesReceiver {
    private String workerId;
    public WorkQueuesReceiver(String workerId) {
        this.workerId = workerId;
    }
    void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection(); Channel channel = connection.createChannel();
        final var durable = true; // survives restart
        channel.queueDeclare("hello_queue_durable", durable, false, false, null);
        System.out.println(workerId + " Waiting for messages. To exit press CTRL+C");

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            final String message = new String(delivery.getBody(), StandardCharsets.UTF_8);
            System.out.println(workerId + " Received '" + message + "'");
            try {
                doWork(message);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            } finally {
                // ensures the message is done and done.
                // without this, the message's state woul be stuck in an "unack" state and the next
                // restart would cause the same message to be processed again.
                channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
            }
        };
        channel.basicConsume("hello_queue_durable", false, deliverCallback, consumerTag -> { });
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

    public void act() throws Exception {
        final var receiver1 = new WorkQueuesReceiver("worker-1");
        final var receiver2 = new WorkQueuesReceiver("worker-2");
        receiver1.act();
        receiver2.act();

        for (int i = 0; i < 10; i++) {
            StringBuilder v = new StringBuilder(i + " message");
            v.append(".".repeat(i));
            sender.act(v.toString());
        }
    }
}
