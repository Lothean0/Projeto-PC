import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class Client {
    public static void main(String[] args) throws IOException {
        BufferedReader cli = new BufferedReader(new InputStreamReader(System.in));

        Socket socket = new Socket("127.0.0.1", Integer.parseInt(args[0]));
        BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        PrintWriter out = new PrintWriter(socket.getOutputStream());

        // Start a new thread to read server responses
        Thread readerThread = new Thread(() -> {
            while (true) {
                try {
                    System.out.println(in.readLine());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        });
        readerThread.start();

        // Main thread handles user input
        String line;
        while ((line = cli.readLine()) != null) {
            if (line.equals("exit")) {
                out.println(line);
                out.flush();
                break;
            }
            out.println(line);
            out.flush();
        }

        // Close resources
        socket.close();
        cli.close();
    }
}