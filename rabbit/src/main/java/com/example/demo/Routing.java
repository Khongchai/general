package com.example.demo;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;
import org.springframework.stereotype.Service;

class EmitLogsDirect {
    private static final String EXCHANGE_NAME = "logs-with-level";

    public void act(String level, String message) throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        try (Connection connection = factory.newConnection();
             Channel channel = connection.createChannel()) {
            channel.exchangeDeclare(EXCHANGE_NAME, "direct");

            channel.basicPublish(EXCHANGE_NAME, level, null, message.getBytes("UTF-8"));
            System.out.println(" [x] Sent '" + message + "'");
        }
    }
}

class ReceiveLogsDirect {
    private static final String EXCHANGE_NAME = "logs-with-level";

    private final String logLevel;

    public ReceiveLogsDirect(String logLevel) {
        this.logLevel = logLevel;
    }

    public void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        channel.exchangeDeclare(EXCHANGE_NAME, "direct");
        // no parameter = non durable, exclusive, autodelete
        String queueName = channel.queueDeclare().getQueue();
        channel.queueBind(queueName, EXCHANGE_NAME, this.logLevel);

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        final DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            String message = new String(delivery.getBody(), "UTF-8");
            System.out.println(" [x] Received '" + message + "'");
        };
        channel.basicConsume(queueName, true, deliverCallback, consumerTag -> { });
    }
}

@Service
public class Routing {
    // basically broadcast with an additional filter.
    public void act() throws Exception {
        final var emitter = new EmitLogsDirect();
        final var receiver1 = new ReceiveLogsDirect("warn");
        final var receiver2 = new ReceiveLogsDirect("info");
        final var receiver3 = new ReceiveLogsDirect("warn");
        receiver1.act();
        receiver2.act();
        receiver3.act();
        emitter.act("warn", "This is a warning");
        emitter.act("info", "This is an info");
    }
}
