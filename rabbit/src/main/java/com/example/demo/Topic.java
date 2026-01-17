package com.example.demo;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;
import org.springframework.stereotype.Service;

class EmitTopicLogs {
    private static final String EXCHANGE_NAME = "topic-logs";

    public void act(String level, String message) throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        try (Connection connection = factory.newConnection();
             Channel channel = connection.createChannel()) {
            channel.exchangeDeclare(EXCHANGE_NAME, "topic");

            channel.basicPublish(EXCHANGE_NAME, level, null, message.getBytes("UTF-8"));
            System.out.println(" [x] Sent '" + level + ":" + message + "'");
        }
    }
}

class ReceiveTopicLogs {
    private static final String EXCHANGE_NAME = "topic-logs";

    private final String logLevel;

    public ReceiveTopicLogs(String logLevel) {
        this.logLevel = logLevel;
    }

    public void act() throws Exception {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        channel.exchangeDeclare(EXCHANGE_NAME, "topic");
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
public class Topic {
    // basically routing, but can target more specific stuff.
    public void act() throws Exception {
        final var emitter = new EmitTopicLogs();
        final var receiver1 = new ReceiveTopicLogs("earth.*.*");
        receiver1.act();
        emitter.act("earth.country.thailand", "Parcel for Taksin");
        emitter.act("mars.country.olympus", "Parcel for Zeus");
        emitter.act("earth.country.usa", "Parcel for Terry Davis");
    }
}
