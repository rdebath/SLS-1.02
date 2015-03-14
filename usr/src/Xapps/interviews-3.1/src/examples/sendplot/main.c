//________________________________________________________________________________
// send - Send a ToolTalk message (notice) to the Request Broker (ttservice)

#include <stream.h>
#include <stdlib.h>
#include "rpc_write.h"

Writer writer(8001);

main()
{
    char string[81];
    float x = 0.0;
    float y = 92.25;
	float delta = -0.25;
	cerr << "Hit return to send the data.\n";
	for (int k=0; k<20; k++) {
        x += 0.25;
		int mod = k % 5;
		if (mod == 0)
		    delta = -delta;
        y += delta;
        sprintf(string, "FOO %5.3f %5.3f ", x, y);
        cerr << string;
        writer.send(string);
		writer.flush();
        getchar();
    }
    writer.send("exit");
	writer.flush();
    return 0;
}
