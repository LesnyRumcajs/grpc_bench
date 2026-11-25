package helloworld;

import jakarta.inject.Singleton;

@Singleton
public class GreetingService {

    String sayHello(String name) {
        return name;
    }
}
