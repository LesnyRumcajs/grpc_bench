package helloworld;

import javax.inject.Singleton;

@Singleton
public class GreetingService {

    String sayHello(String name) {
        return name;
    }
}
