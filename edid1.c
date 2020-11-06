// sudo apt-get install libgstreamer-plugins-base1.0-dev

// gcc basic-tutorial-1.c -o basic-tutorial-1 `pkg-config --cflags --libs gstreamer-1.0`
// gcc basic-tutorial-1.c -o basic-tutorial-1 `pkg-config --cflags --libs gstreamer-1.0  gstreamer-app-1.0`

/* 
    runCommand $ "/usr/bin/gst-launch-1.0
    -v udpsrc multicast-group=" <> (T.unpack ip) <> " auto-multicast=true port="<> (T.unpack port) <>" 
    ! \"application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)RAW, sampling=(string)YCbCr-4:2:2, depth=(string)8, width=(string)320, height=(string)180, ssrc=(uint)1234, payload=(int)112\" 
    ! rtpjitterbuffer 
    ! rtpvrawdepay 
    ! videoconvert 
    ! queue 
    ! jpegenc 
    ! multifilesink location=preview/imgs/" <> (T.unpack tx) <> "/%d  max-files=10"
*/

#include <gst/gst.h>
#include <gst/app/gstappsink.h>

typedef unsigned char byte;

typedef struct _CustomData {
  int* cgi;
  int* cgs0;
  int* cgs1;
  int* cgs2;
  byte* cga0;
  byte* cga1;
  byte* cga2;
  GMainLoop   *loop; 
  GstElement  *pipeline;
} CustomData;
         
int
on_new_sample_from_sink (GstAppSink * elt, CustomData *data)
{
	GstSample *sample;
	GstBuffer *buffer;
	GstMapInfo map;
	
	/* get the sample from appsink */
	sample = gst_app_sink_pull_sample(elt);
	buffer = gst_sample_get_buffer(sample);

  if(*(data->cgi) == -2)
  { 
    g_print ("l1");
    g_main_loop_quit (data->loop);
    
    /*
    g_print ("l1a");
    gst_element_set_state (data->pipeline, GST_STATE_NULL);
    //g_print ("l1b");
    gst_object_unref (GST_OBJECT (data->pipeline));
    g_print ("l1c");
    g_main_loop_unref (data->loop);
    g_print ("l2");

    g_print ("l1");
    gst_element_set_state (data->pipeline, GST_STATE_NULL);
    */  
    g_print ("l2");

    return 0;
  }
  

	/* make a copy */
  if (gst_buffer_map (buffer, &map, GST_MAP_READ)) {
		g_print("%ld\n", map.size);

    int idx =  (*(data->cgi) + 1) % 3;
    
    //g_print("%d\n", (*(data->cgi)));
    
    if(idx == 0)
    {
      *(data->cgs0) = map.size;
      memcpy(data->cga0, map.data, map.size);
    } 
    else if (idx == 1)
    {
      *(data->cgs1) = map.size;
      memcpy(data->cga1, map.data, map.size);
    } 
    else if (idx == 2)
    {
      *(data->cgs2) = map.size;
      memcpy(data->cga2, map.data, map.size);
    }

    *(data->cgi) = (*(data->cgi) + 1) % 3;
    
		gst_buffer_unmap (buffer, &map);
    
  }
	/* we don't need the appsink sample anymore */
	gst_sample_unref (sample);
	
	return 0;
}

int
stope  (long* ptr)
{
  g_print ("l1a");
  gst_element_set_state (*ptr, GST_STATE_NULL);
  gst_object_unref (*ptr);
  g_print ("l1b");
  return 0;
}


int
mainloop  ( long* ptr
          , char * ip
          , int port
          , int* ii11 
          , int* size00
          , int* size11
          , int* size22
          , byte* a0 
          , byte* a1
          , byte* a2 )
{
    GMainLoop   *loop; 
    GstElement  *pipeline
              , *udpsrc
              , *capsfilter
              , *rtpjitterbuffer
              , *rtpvrawdepay 
              , *videoconvert
              , *queue
              , *jpegenc
              , *multifilesink
              , *appsink;

    CustomData data;

    // init GStreamer
    // gint   argc
    // gchar *argv[]
    gint   argc = 1;
    //gchar argv[1] = {"exec"};
    //argv = ["xeec"];

    gst_init (&argc, 0);
    loop = g_main_loop_new (NULL, FALSE);

    // setup pipeline
    pipeline = gst_pipeline_new ("pipeline");

    data.pipeline = pipeline;
    data.loop = loop;
    data.cgi = ii11;
    data.cgs0 = size00;
    data.cgs1 = size11;
    data.cgs2 = size22;
    data.cga0 = a0;
    data.cga1 = a1;
    data.cga2 = a2;

    *data.cgi = -1;


    *ptr = (long)pipeline;

    /////////////////////
    udpsrc        = gst_element_factory_make("udpsrc",            "src");
    capsfilter = gst_element_factory_make("capsfilter", NULL);
    GstCaps *caps = gst_caps_from_string("application/x-rtp, media=(string)video, clock-rate=(int)90000, encoding-name=(string)RAW, sampling=(string)YCbCr-4:2:2, depth=(string)8, width=(string)320, height=(string)180, ssrc=(uint)1234, payload=(int)112");
    rtpjitterbuffer = gst_element_factory_make("rtpjitterbuffer", NULL);
    rtpvrawdepay  = gst_element_factory_make("rtpvrawdepay", NULL);
    videoconvert = gst_element_factory_make("videoconvert", NULL);
    queue = gst_element_factory_make("queue", NULL);
    jpegenc = gst_element_factory_make("jpegenc", NULL);
    multifilesink = gst_element_factory_make("multifilesink", NULL);
    appsink = gst_element_factory_make("appsink", NULL);
    
    //g_object_set(udpsrc, "port", 6973, NULL);
    g_object_set(udpsrc, "port", port, NULL);
    g_object_set(udpsrc, "multicast-group", ip, NULL); 
    g_object_set(udpsrc, "auto-multicast", 1, NULL);

    g_object_set(capsfilter, "caps", caps, NULL);

    g_object_set(appsink, "emit-signals", TRUE, "sync", FALSE, NULL);
  	g_signal_connect (appsink, "new-sample", G_CALLBACK (on_new_sample_from_sink), &data);

    g_print("%s %d go on0!\n", ip, port);
    /////////////////////
    if (!pipeline 
        || !udpsrc 
        || !capsfilter 
        || !rtpjitterbuffer 
        || !rtpvrawdepay 
        || !videoconvert 
        || !queue
        || !jpegenc
        || !appsink
        ) {
      g_error("Failed to create elements");
      return -1;
    }

    gst_bin_add_many (GST_BIN (pipeline)
        , udpsrc 
        , capsfilter
        , rtpjitterbuffer 
        , rtpvrawdepay 
        , videoconvert 
        , queue
        , jpegenc
        , appsink
      , NULL);

    if ( !gst_element_link_many(udpsrc, capsfilter, rtpjitterbuffer, rtpvrawdepay, queue, jpegenc, appsink, NULL) 
        ) {
        g_error("Failed to link elements");
        return -2;
    }
    
    //g_print("go on!\n");
    // play
    gst_element_set_state (pipeline, GST_STATE_PLAYING);
    
    g_main_loop_run (loop);

    // clean up
    gst_element_set_state (pipeline, GST_STATE_NULL);
    gst_object_unref (GST_OBJECT (pipeline));
    g_main_loop_unref (loop);

    return 0;
}

