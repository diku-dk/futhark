/* The simple Vulkan runtime framework used by Futhark. */

#include <vulkan/vulkan.h>
#include <stdlib.h>

#define VULKAN_SUCCEED(e) vulkan_succeed(e, #e, __FILE__, __LINE__)

#define SCALAR_TAG "scalar"
#define ALLOC_STEP 32

struct vulkan_config {
  uint32_t api_version;
  int debugging;
  int logging;
  int preferred_device_index;
  int preferred_device_queue_index;

  const char* dump_program_to;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_threshold;
  size_t transpose_block_dim;

  int default_group_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  size_t *size_values;
  const char **size_classes;
};

void vulkan_config_init(struct vulkan_config *cfg,
                        int num_sizes,
                        const char *size_names[],
                        size_t *size_values,
                        const char *size_classes[]) {
  cfg->api_version = VK_MAKE_VERSION(1, 1, 0);
  cfg->debugging = 0;
  cfg->logging = 0;
  cfg->preferred_device_index = -1;
  cfg->preferred_device_queue_index = -1;
  cfg->dump_program_to = NULL;

  cfg->default_group_size = 256;
  cfg->default_num_groups = 128;
  cfg->default_tile_size = 32;
  cfg->default_threshold = 32*1024;
  cfg->transpose_block_dim = 16;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed = 0;

  cfg->num_sizes = num_sizes;
  cfg->size_names = size_names;
  cfg->size_values = size_values;
  cfg->size_classes = size_classes;
}

struct vulkan_pipeline_cache_entry {
  VkPipeline pipeline;
  void *key;
};

struct vulkan_shader_context {
  const char *entry_point;
  VkShaderModule shader_module;
  VkDescriptorSetLayout descriptor_set_layout;
  VkPipelineLayout pipeline_layout;

  struct vulkan_pipeline_cache_entry *pipeline_cache;
  uint32_t pipeline_cache_count;
};

struct vulkan_command_buffer_context {
  VkCommandBuffer command_buffer;
  VkFence fence;
  VkDescriptorSet descriptor_set;
  uint8_t active_descriptor_set;
  uint32_t owns;
  struct vk_buffer_mem_pair *scalars;
  uint32_t scalar_count;
};

struct vulkan_context {
  VkInstance instance;
  VkPhysicalDevice physical_device;
  VkDevice device;
  VkQueue queue;
  uint32_t queue_family_index;
  VkCommandPool command_pool;
  uint32_t max_descriptor_set_size;
  VkDescriptorPool *descriptor_pools;

  struct vulkan_command_buffer_context *command_buffers;
  uint32_t command_buffer_count;

  struct vulkan_config cfg;

  struct free_list free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;

  size_t lockstep_width;
};

// Error codes taken directly from specification
// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkResult.html
static const char* vulkan_error_string(unsigned int err)
{
    switch (err) {
        case VK_SUCCESS:                            return "Command successfully completed";
        case VK_NOT_READY:                          return "A fence or query has not yet completed";
        case VK_TIMEOUT:                            return "A wait operation has not completed in the specified time";
        case VK_EVENT_SET:                          return "An event is signaled";
        case VK_EVENT_RESET:                        return "An event is unsignaled";
        case VK_INCOMPLETE:                         return "A return array was too small for the result";
        case VK_ERROR_OUT_OF_DEVICE_MEMORY :        return "A device memory allocation has failed";
        case VK_ERROR_INITIALIZATION_FAILED:        return "Initialization of an object could not be completed for implementation-specific reasons";
        case VK_ERROR_DEVICE_LOST:                  return "The logical or physical device has been lost";
        case VK_ERROR_MEMORY_MAP_FAILED:            return "Mapping of a memory object has failed";
        case VK_ERROR_LAYER_NOT_PRESENT:            return "A requested layer is not present or could not be loaded";
        case VK_ERROR_EXTENSION_NOT_PRESENT:        return "A requested extension is not supported";
        case VK_ERROR_FEATURE_NOT_PRESENT:          return "A requested feature is not supported";
        case VK_ERROR_INCOMPATIBLE_DRIVER:          return "The requested version of Vulkan is not supported by the driver or is otherwise incompatible for implementation-specific reasons";
        case VK_ERROR_TOO_MANY_OBJECTS:             return "Too many objects of the type have already been created";
        case VK_ERROR_FORMAT_NOT_SUPPORTED:         return "A requested format is not supported on this device";
        case VK_ERROR_FRAGMENTED_POOL:              return "A pool allocation has failed due to fragmentation of the pool’s memory";
        case VK_ERROR_OUT_OF_POOL_MEMORY:           return "A pool memory allocation has failed";
        case VK_ERROR_INVALID_EXTERNAL_HANDLE:      return "An external handle is not a valid handle of the specified type";
        case VK_ERROR_SURFACE_LOST_KHR:             return "A surface is no longer available";
        case VK_ERROR_NATIVE_WINDOW_IN_USE_KHR:     return "The requested window is already in use by Vulkan or another API in a manner which prevents it from being used again";
        case VK_SUBOPTIMAL_KHR:                     return "A swapchain no longer matches the surface properties exactly, but can still be used to present to the surface successfully";
        case VK_ERROR_OUT_OF_DATE_KHR:              return "A surface has changed in such a way that it is no longer compatible with the swapchain, and further presentation requests using the swapchain will fail";
        case VK_ERROR_INCOMPATIBLE_DISPLAY_KHR:     return "The display used by a swapchain does not use the same presentable image layout, or is incompatible in a way that prevents sharing an image";
        case VK_ERROR_INVALID_SHADER_NV:            return "One or more shaders failed to compile or link";
        case VK_ERROR_FRAGMENTATION_EXT:            return "A descriptor pool creation has failed due to fragmentation";
        default:                                    return "Unknown";
    }
}

static void vulkan_succeed(unsigned int ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret != VK_SUCCESS) {
    panic(-1, "%s:%d: Vulkan call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, vulkan_error_string(ret));
  }
}

static int get_suitable_physical_device(const struct vulkan_config *cfg, VkPhysicalDevice *const devices,
                               uint32_t device_count, VkPhysicalDevice* device) {
  for (uint32_t i = 0; i < device_count; ++i)
  {
    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[i], &properties);

    if (properties.apiVersion < cfg->api_version)
      continue;

    *device = devices[i];
    return 1;
  }
  return 0;
}

static VkPhysicalDevice get_preferred_physical_device(const struct vulkan_config *cfg, VkInstance instance) {
  uint32_t device_count = 0;
  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, 0));

  VkPhysicalDevice *const devices = malloc(sizeof(VkPhysicalDevice) * device_count);

  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, devices));

  VkPhysicalDevice device;
  if (cfg->preferred_device_index >= 0) {
    if(cfg->preferred_device_index >= device_count)
      panic(1, "Specified device index not found.\n");

    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[cfg->preferred_device_index], &properties);

    if (properties.apiVersion < cfg->api_version)
      panic(1, "Device at specified index does not support required Vulkan API.");
    
    device = devices[cfg->preferred_device_index];
  }
  else if(!get_suitable_physical_device(cfg, devices, device_count, &device)) {
    panic(1, "Could not find acceptable Vulkan device.\n");
  }

  free(devices);
  return device;
}

static uint32_t get_suitable_queue_family(const struct vulkan_config *cfg,
                                          VkQueueFamilyProperties *const queue_props,
                                          uint32_t queue_prop_count,
                                          uint32_t *queue_family_index) {
  for (uint32_t i = 0; i < queue_prop_count; ++i)
  {
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_props[i].queueFlags);

      if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      {
          *queue_family_index = i;
          return 1;
      }
  }
  
  return 0;
}

static uint32_t get_preferred_queue_family(const struct vulkan_config *cfg, VkPhysicalDevice physical_device) {
  uint32_t queue_family_props_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, 0);

  VkQueueFamilyProperties *const queue_family_props = malloc(sizeof(VkQueueFamilyProperties) * queue_family_props_count);

  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, queue_family_props);

  uint32_t queue_family_index;
  if (cfg->preferred_device_queue_index >= 0) {
    if(cfg->preferred_device_queue_index >= queue_family_props_count)
      panic(1, "Specified queue family index not found.\n");
      
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_family_props[cfg->preferred_device_queue_index].queueFlags);

    if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      panic(1, "Queue family does not support compute shaders.");
    
    queue_family_index = cfg->preferred_device_queue_index;
  }
  else if(!get_suitable_queue_family(cfg, queue_family_props, queue_family_props_count, &queue_family_index)) {
    panic(1, "Could not find acceptable device queue.\n");
  }
  
  free(queue_family_props);
  return queue_family_index;
}

static void init_sizes(struct vulkan_context *ctx) {
  VkPhysicalDeviceProperties properties;
  vkGetPhysicalDeviceProperties(ctx->physical_device, &properties);

  size_t max_group_size = properties.limits.maxComputeWorkGroupSize[0];

  size_t max_tile_size = sqrt(max_group_size);

  if (max_group_size < ctx->cfg.default_group_size) {
    if (ctx->cfg.default_group_size_changed) {
      fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
              max_group_size, ctx->cfg.default_group_size);
    }
    ctx->cfg.default_group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg.default_tile_size) {
    if (ctx->cfg.default_tile_size_changed) {
      fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
              max_tile_size, ctx->cfg.default_tile_size);
    }
    ctx->cfg.default_tile_size = max_tile_size;
  }

  ctx->max_group_size = max_group_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      panic(1, "Unknown size class for size '%s': %s\n", size_name, size_class);
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }
}

static VkResult vulkan_allocate_command_buffers(struct vulkan_context *ctx,
                                                uint32_t offset,
                                                uint32_t count) {

  VkCommandBuffer *tmp_buffers = malloc(count * sizeof(VkCommandBuffer));

  VkCommandBufferAllocateInfo command_buffer_allocate_info;
  command_buffer_allocate_info.sType              = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  command_buffer_allocate_info.pNext              = 0;
  command_buffer_allocate_info.commandPool        = ctx->command_pool;
  command_buffer_allocate_info.level              = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  command_buffer_allocate_info.commandBufferCount = count;

  VkResult error = vkAllocateCommandBuffers(ctx->device, &command_buffer_allocate_info, tmp_buffers);

  if (error != VK_SUCCESS)
    return error;

  const VkFenceCreateInfo fence_create_info = {
    VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
    0,
    VK_FENCE_CREATE_SIGNALED_BIT
  };

  for (int i = offset; i < offset + count; ++i) {
    ctx->command_buffers[i].active_descriptor_set = 0;
    ctx->command_buffers[i].owns = 0;
    ctx->command_buffers[i].scalar_count = 0;
    ctx->command_buffers[i].command_buffer = tmp_buffers[i-offset]; 

    error = vkCreateFence(ctx->device,
                          &fence_create_info,
                          0,
                          &ctx->command_buffers[i].fence);

    if (error != VK_SUCCESS)
      return error;
  }

  free(tmp_buffers);
  return VK_SUCCESS;
}

VkResult vulkan_create_descriptor_pool(struct vulkan_context *ctx, VkDescriptorPool *descriptor_pool) {
  VkDescriptorPoolSize descriptor_pool_size;
  descriptor_pool_size.type = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
  descriptor_pool_size.descriptorCount = ctx->max_descriptor_set_size * ALLOC_STEP;

  VkDescriptorPoolCreateInfo descriptor_pool_create_info;
  descriptor_pool_create_info.sType         = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  descriptor_pool_create_info.pNext         = 0;
  descriptor_pool_create_info.flags         = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT;
  descriptor_pool_create_info.maxSets       = ALLOC_STEP;
  descriptor_pool_create_info.poolSizeCount = 1;
  descriptor_pool_create_info.pPoolSizes    = &descriptor_pool_size;

  return vkCreateDescriptorPool(ctx->device, &descriptor_pool_create_info, 0, descriptor_pool);
}

static void setup_vulkan(struct vulkan_context *ctx, uint32_t max_desc_count) {
  ctx->lockstep_width = 1;

  free_list_init(&ctx->free_list);

  VkApplicationInfo application_info;
  application_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  application_info.pNext              = 0;
  application_info.pApplicationName   = "";
  application_info.applicationVersion = 0;
  application_info.pEngineName        = "Futhark";
  application_info.engineVersion      = 0;
  application_info.apiVersion         = ctx->cfg.api_version;

  VkInstanceCreateInfo instance_create_info;
  instance_create_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  instance_create_info.pNext                   = 0;
  instance_create_info.flags                   = 0;
  instance_create_info.pApplicationInfo        = &application_info;
  instance_create_info.enabledLayerCount       = 0;
  instance_create_info.ppEnabledLayerNames     = 0;
  instance_create_info.enabledExtensionCount   = 0;
  instance_create_info.ppEnabledExtensionNames = 0;

  VULKAN_SUCCEED(vkCreateInstance(&instance_create_info, 0, &ctx->instance));

  ctx->physical_device = get_preferred_physical_device(&ctx->cfg, ctx->instance);

  init_sizes(ctx);

  ctx->queue_family_index = get_preferred_queue_family(&ctx->cfg, ctx->physical_device);

  const float queue_prio = 1.0f;
  VkDeviceQueueCreateInfo device_queue_create_info;
  device_queue_create_info.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  device_queue_create_info.pNext            = 0;
  device_queue_create_info.flags            = 0;
  device_queue_create_info.queueFamilyIndex = ctx->queue_family_index;
  device_queue_create_info.queueCount       = 1;
  device_queue_create_info.pQueuePriorities = &queue_prio;
  
  const char *device_extension_names[] = { "VK_KHR_8bit_storage" };

  VkPhysicalDevice8BitStorageFeaturesKHR storage_8bit_features;
  storage_8bit_features.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR;
  storage_8bit_features.pNext                             = 0;
  storage_8bit_features.storageBuffer8BitAccess           = true;

  VkPhysicalDeviceFeatures device_features = {};
  device_features.shaderInt16   = true;
  device_features.shaderInt64   = true;
  device_features.shaderFloat64 = true;

  VkDeviceCreateInfo device_create_info;
  device_create_info.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  device_create_info.pNext                   = &storage_8bit_features;
  device_create_info.flags                   = 0;
  device_create_info.queueCreateInfoCount    = 1;
  device_create_info.pQueueCreateInfos       = &device_queue_create_info;
  device_create_info.enabledLayerCount       = 0;
  device_create_info.ppEnabledLayerNames     = 0;
  device_create_info.enabledExtensionCount   = sizeof(device_extension_names) / sizeof(char*);
  device_create_info.ppEnabledExtensionNames = device_extension_names;
  device_create_info.pEnabledFeatures        = &device_features;

  VULKAN_SUCCEED(vkCreateDevice(ctx->physical_device, &device_create_info, 0, &ctx->device));

  vkGetDeviceQueue(ctx->device, ctx->queue_family_index, 0, &ctx->queue);

  VkCommandPoolCreateInfo command_pool_create_info;
  command_pool_create_info.sType            = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  command_pool_create_info.pNext            = 0;
  command_pool_create_info.flags            = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  command_pool_create_info.queueFamilyIndex = ctx->queue_family_index;

  VULKAN_SUCCEED(vkCreateCommandPool(ctx->device, &command_pool_create_info, 0, &ctx->command_pool));

  ctx->command_buffer_count = ALLOC_STEP;
  ctx->command_buffers = malloc(ctx->command_buffer_count * sizeof(struct vulkan_command_buffer_context));
  VULKAN_SUCCEED(vulkan_allocate_command_buffers(ctx, 0, ctx->command_buffer_count));

  ctx->max_descriptor_set_size = max_desc_count;

  if(max_desc_count) {
    ctx->descriptor_pools = malloc(sizeof(VkDescriptorPool));
    VULKAN_SUCCEED(vulkan_create_descriptor_pool(ctx, ctx->descriptor_pools));
  }
}

void vulkan_setup_shader(struct vulkan_context *ctx,
                         struct vulkan_shader_context *sh_ctx,
                         const char* entry_point,
                         uint32_t descriptor_count,
                         const uint32_t shader[],
                         uint32_t shader_size) {
  
  if (ctx->cfg.dump_program_to != NULL) {
    char *fname = malloc((strlen(ctx->cfg.dump_program_to) + strlen(entry_point) + 5) * sizeof(char));
    sprintf(fname, "%s.%s.spv", ctx->cfg.dump_program_to, entry_point);
    FILE *f = fopen(fname, "wb");
    assert(f != NULL);
    fwrite(shader, sizeof(uint32_t), shader_size / sizeof(uint32_t), f);
    fflush(f);
    fclose(f);
    free(fname);
  }

  sh_ctx->entry_point = entry_point;
  sh_ctx->pipeline_cache_count = 0;
  sh_ctx->pipeline_cache = NULL;

  VkShaderModuleCreateInfo shader_module_create_info;
  shader_module_create_info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  shader_module_create_info.pNext    = 0;
  shader_module_create_info.flags    = 0;
  shader_module_create_info.codeSize = shader_size;
  shader_module_create_info.pCode    = shader;

  VULKAN_SUCCEED(vkCreateShaderModule(ctx->device, &shader_module_create_info, 0, &sh_ctx->shader_module));

  VkDescriptorSetLayoutBinding *desc_set_layout_bindings =
    malloc(sizeof(VkDescriptorSetLayoutBinding) * descriptor_count);
  
  for (int i = 0; i < descriptor_count; ++i) {
    desc_set_layout_bindings[i].binding = i;
    desc_set_layout_bindings[i].descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
    desc_set_layout_bindings[i].descriptorCount = 1;
    desc_set_layout_bindings[i].stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
    desc_set_layout_bindings[i].pImmutableSamplers = 0;
  }

  VkDescriptorSetLayoutCreateInfo desc_set_layout_create_info;
  desc_set_layout_create_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  desc_set_layout_create_info.pNext = 0;
  desc_set_layout_create_info.flags = 0;
  desc_set_layout_create_info.bindingCount = descriptor_count;
  desc_set_layout_create_info.pBindings = desc_set_layout_bindings;

  VULKAN_SUCCEED(vkCreateDescriptorSetLayout(ctx->device,
                                             &desc_set_layout_create_info,
                                             0,
                                             &sh_ctx->descriptor_set_layout));
  free(desc_set_layout_bindings);

  VkPipelineLayoutCreateInfo pipeline_layout_create_info;
  pipeline_layout_create_info.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  pipeline_layout_create_info.pNext                  = 0;
  pipeline_layout_create_info.flags                  = 0;
  pipeline_layout_create_info.setLayoutCount         = 1;
  pipeline_layout_create_info.pSetLayouts            = &sh_ctx->descriptor_set_layout;
  pipeline_layout_create_info.pushConstantRangeCount = 0;
  pipeline_layout_create_info.pPushConstantRanges    = 0;

  VULKAN_SUCCEED(vkCreatePipelineLayout(ctx->device,
                                        &pipeline_layout_create_info,
                                        0,
                                        &sh_ctx->pipeline_layout));
}

void vulkan_create_pipeline(struct vulkan_context *ctx,
                            struct vulkan_shader_context *sh_ctx,
                            const VkSpecializationInfo spec_info,
                            VkPipeline *pipeline) {
  VkComputePipelineCreateInfo compute_pipeline_create_info;
  compute_pipeline_create_info.sType                     = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
  compute_pipeline_create_info.pNext                     = 0;
  compute_pipeline_create_info.flags                     = 0;
  compute_pipeline_create_info.layout                    = sh_ctx->pipeline_layout;
  compute_pipeline_create_info.basePipelineHandle        = 0;
  compute_pipeline_create_info.basePipelineIndex         = 0;
  compute_pipeline_create_info.stage.sType               = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
  compute_pipeline_create_info.stage.pNext               = 0;
  compute_pipeline_create_info.stage.flags               = 0;
  compute_pipeline_create_info.stage.stage               = VK_SHADER_STAGE_COMPUTE_BIT;
  compute_pipeline_create_info.stage.module              = sh_ctx->shader_module;
  compute_pipeline_create_info.stage.pName               = sh_ctx->entry_point;
  compute_pipeline_create_info.stage.pSpecializationInfo = &spec_info;

  VULKAN_SUCCEED(vkCreateComputePipelines(ctx->device,
                                          0,
                                          1,
                                          &compute_pipeline_create_info,
                                          0,
                                          pipeline));
}

void vulkan_destroy_pipeline(struct vulkan_context *ctx, VkPipeline pipeline) {
  vkDestroyPipeline(ctx->device, pipeline, 0);
}

void vulkan_add_pipeline_cache_entry(struct vulkan_shader_context *sh_ctx,
                                     void *cache_key,
                                     size_t cache_key_size,
                                     VkPipeline pipeline) {
  uint32_t old_cache_count = sh_ctx->pipeline_cache_count++;

  sh_ctx->pipeline_cache = realloc(sh_ctx->pipeline_cache,
                                    sh_ctx->pipeline_cache_count *
                                    sizeof(struct vulkan_pipeline_cache_entry));

  sh_ctx->pipeline_cache[old_cache_count].pipeline = pipeline;
  sh_ctx->pipeline_cache[old_cache_count].key = malloc(cache_key_size);
  memcpy(sh_ctx->pipeline_cache[old_cache_count].key, cache_key, cache_key_size);
}

uint8_t vulkan_find_cached_pipeline(struct vulkan_shader_context *sh_ctx,
                                    void *cache_key,
                                    size_t cache_key_size,
                                    VkPipeline *cached_pipeline) {
  for (int i = 0; i < sh_ctx->pipeline_cache_count; ++i) {
    if (memcmp(sh_ctx->pipeline_cache[i].key, cache_key, cache_key_size) == 0) {
      *cached_pipeline = sh_ctx->pipeline_cache[i].pipeline;
      return 1;
    }
  }

  return 0;
}

VkResult vulkan_sync_mem(struct vulkan_context *ctx, struct vk_buffer_mem_pair *mem) {
  if (!mem->owned)
    return VK_SUCCESS;

  // Why is vkWaitForFences not working?
  VkResult error = VK_TIMEOUT;
  while(error == VK_TIMEOUT)
    error = vkWaitForFences(ctx->device, 1, &ctx->command_buffers[mem->owner_index].fence, VK_TRUE, 1e15);

  if (error != VK_SUCCESS)
    return error;
  
  // The corresponding command buffer no longer references the memory
  ctx->command_buffers[mem->owner_index].owns--;

  mem->owned = 0;
  return VK_SUCCESS;
}

VkResult vulkan_free_mem_sync(struct vulkan_context *ctx, struct vk_buffer_mem_pair *mem) {
  VkResult error = vulkan_sync_mem(ctx, mem);

  if (error != VK_SUCCESS)
    return error;

  vkFreeMemory(ctx->device, mem->memory, 0);
  vkDestroyBuffer(ctx->device, mem->buffer, 0);
  return VK_SUCCESS;
}

VkResult vulkan_alloc(struct vulkan_context *ctx,
                      size_t min_size,
                      const char *tag,
                      struct vk_buffer_mem_pair *mem_out) {
  assert(min_size >= 0);
  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  VkBufferCreateInfo buffer_create_info;
  buffer_create_info.sType                 = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  buffer_create_info.pNext                 = 0;
  buffer_create_info.flags                 = 0;
  buffer_create_info.size                  = min_size;
  buffer_create_info.usage                 = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
                                           | VK_BUFFER_USAGE_TRANSFER_SRC_BIT
                                           | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
  buffer_create_info.sharingMode           = VK_SHARING_MODE_EXCLUSIVE;
  buffer_create_info.queueFamilyIndexCount = 1;
  buffer_create_info.pQueueFamilyIndices   = &ctx->queue_family_index;

  VkResult error = vkCreateBuffer(ctx->device, &buffer_create_info, 0, &mem_out->buffer);

  // If buffer creation fails we cannot recover
  if (error != VK_SUCCESS) {
    return error;
  }
  
  VkMemoryRequirements mem_req;
  vkGetBufferMemoryRequirements(ctx->device, mem_out->buffer, &mem_req);

  mem_out->size = mem_req.size;

  size_t size;
  struct vk_buffer_mem_pair mem;
  if (free_list_find(&ctx->free_list, tag, &size, &mem) == 0) {
    // Successfully found a free block.  Is it big enough?
    // FIXME: See opencl_alloc in opencl.h
    if (size >= mem_req.size) {
      vkDestroyBuffer(ctx->device, mem_out->buffer, 0);
      *mem_out = mem;
      return VK_SUCCESS;
    }

    // Not just right - free it.
    vulkan_free_mem_sync(ctx, &mem);
  }

  // Find suitable heap
  
  VkPhysicalDeviceMemoryProperties properties;
  vkGetPhysicalDeviceMemoryProperties(ctx->physical_device, &properties);

  uint32_t mem_type_i = VK_MAX_MEMORY_TYPES;
  for (uint32_t k = 0; k < properties.memoryTypeCount; k++)
  {
      if ((VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT & properties.memoryTypes[k].propertyFlags) &&
          (VK_MEMORY_PROPERTY_HOST_COHERENT_BIT & properties.memoryTypes[k].propertyFlags) &&
          (mem_req.size < properties.memoryHeaps[properties.memoryTypes[k].heapIndex].size))
      {
          mem_type_i = k;
          break;
      }
  }

  // We have to allocate a new block from the driver.  If the
  // allocation does not succeed, then we might be in an out-of-memory
  // situation.  We now start freeing things from the free list until
  // we think we have freed enough that the allocation will succeed.
  // Since we don't know how far the allocation is from fitting, we
  // have to check after every deallocation.  This might be pretty
  // expensive.  Let's hope that this case is hit rarely.

  mem_out->owned = 0;
  mem_out->owner_index = 0;
  
  VkMemoryAllocateInfo mem_alloc_info;
  mem_alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  mem_alloc_info.pNext           = 0;
  mem_alloc_info.allocationSize  = mem_req.size;
  mem_alloc_info.memoryTypeIndex = mem_type_i;

  error = vkAllocateMemory(ctx->device, &mem_alloc_info, 0, &mem_out->memory);

  while (error != VK_SUCCESS) {
    if (free_list_first(&ctx->free_list, &mem) != 0)
      break;
    vulkan_free_mem_sync(ctx, &mem);
    error = vkAllocateMemory(ctx->device, &mem_alloc_info, 0, &mem_out->memory);
  }

  return vkBindBufferMemory(ctx->device, mem_out->buffer, mem_out->memory, 0);
}

void vulkan_free(struct vulkan_context *ctx, struct vk_buffer_mem_pair mem, const char *tag) {
  struct vk_buffer_mem_pair existing_mem;

  // If there is already a block with this tag, then remove it.
  size_t size;
  if (free_list_find(&ctx->free_list, tag, &size, &existing_mem) == 0)
    vulkan_free_mem_sync(ctx, &existing_mem);

  free_list_insert(&ctx->free_list, mem.size, mem, tag);
}

void vulkan_free_all(struct vulkan_context *ctx) {
  struct vk_buffer_mem_pair mem;
  free_list_pack(&ctx->free_list);
  while (free_list_first(&ctx->free_list, &mem) == 0)
    vulkan_free_mem_sync(ctx, &mem);
}

struct vk_buffer_mem_pair* vulkan_alloc_scalars(struct vulkan_context *ctx,
                                                uint32_t command_buffer_index,
                                                uint32_t count,
                                                size_t *sizes) {
  if (count < 1)
    return NULL;

  struct vulkan_command_buffer_context *command_buffer_ctx = ctx->command_buffers + command_buffer_index;

  command_buffer_ctx->scalar_count = count;
  command_buffer_ctx->scalars = malloc(count * sizeof(struct vk_buffer_mem_pair));

  for (int i = 0; i < count; ++i)
    vulkan_alloc(ctx, sizes[i], SCALAR_TAG, command_buffer_ctx->scalars + i);

  return command_buffer_ctx->scalars;
}

void vulkan_free_scalars(struct vulkan_context *ctx, uint32_t command_buffer_index) {
  struct vulkan_command_buffer_context *command_buffer_ctx = ctx->command_buffers + command_buffer_index;

  for (int i = 0; i < command_buffer_ctx->scalar_count; ++i)
    vulkan_free(ctx, command_buffer_ctx->scalars[i], SCALAR_TAG);

  if (command_buffer_ctx->scalar_count > 0)
    free(command_buffer_ctx->scalars);
  
  command_buffer_ctx->scalar_count = 0;
}

void vulkan_free_all_scalars(struct vulkan_context *ctx) {
  for (int i = 0; i < ctx->command_buffer_count; ++i)
    vulkan_free_scalars(ctx, i);
}

void vulkan_transfer_ownership(struct vulkan_context *ctx,
                              struct vk_buffer_mem_pair *mem,
                              uint32_t command_buffer_index) {
  if (mem->owned)
    ctx->command_buffers[mem->owner_index].owns--;
  mem->owned = 1;
  mem->owner_index = command_buffer_index;
  ctx->command_buffers[mem->owner_index].owns++;
}

VkResult vulkan_available_command_buffer(struct vulkan_context *ctx,
                                         uint32_t *command_buffer_index) {
                                           
  for (int i = 0; i < ctx->command_buffer_count; ++i) {
    struct vulkan_command_buffer_context *command_buffer_ctx = ctx->command_buffers + i;

    // Skip if still referenced
    if (command_buffer_ctx->owns > 0)
      continue;

    // Skip if still executing
    VkResult fence_status = vkGetFenceStatus(ctx->device, command_buffer_ctx->fence);
    if (fence_status != VK_SUCCESS)
      continue;

    // Free up old scalars
    vulkan_free_scalars(ctx, i);

    *command_buffer_index = i;
    return vkResetFences(ctx->device, 1, &command_buffer_ctx->fence);
  }

  uint32_t old_command_buffer_count = ctx->command_buffer_count;

  ctx->command_buffer_count += ALLOC_STEP;
  ctx->command_buffers =
    realloc(ctx->command_buffers, ctx->command_buffer_count * sizeof(struct vulkan_command_buffer_context));

  VkResult error = vulkan_allocate_command_buffers(ctx, old_command_buffer_count, ALLOC_STEP);

  if (error != VK_SUCCESS)
    return error;

  ctx->descriptor_pools = realloc(ctx->descriptor_pools,
                                  ctx->command_buffer_count / ALLOC_STEP * sizeof(VkDescriptorPool));
  error = vulkan_create_descriptor_pool(ctx, ctx->descriptor_pools + old_command_buffer_count / ALLOC_STEP);
  
  if (error != VK_SUCCESS)
    return error;

  *command_buffer_index = old_command_buffer_count;
  return vkResetFences(ctx->device, 1, &ctx->command_buffers[old_command_buffer_count].fence);
}

void vulkan_free_all_command_buffers(struct vulkan_context *ctx) {
  VkCommandBuffer *tmp_buffers = malloc(ctx->command_buffer_count * sizeof(VkCommandBuffer));

  for (int i = 0; i < ctx->command_buffer_count; ++i){
    vulkan_free_scalars(ctx, i);
    vkDestroyFence(ctx->device, ctx->command_buffers[i].fence, 0);
    tmp_buffers[i] = ctx->command_buffers[i].command_buffer;

    if (i%ALLOC_STEP == 0 && ctx->max_descriptor_set_size)
      vkDestroyDescriptorPool(ctx->device, ctx->descriptor_pools[i/ALLOC_STEP], 0);
  }

  vkFreeCommandBuffers(ctx->device, ctx->command_pool, ctx->command_buffer_count, tmp_buffers);
  free(tmp_buffers);
}

VkResult vulkan_allocate_descriptor_set(struct vulkan_context *ctx,
                                        struct vulkan_shader_context *sh_ctx,
                                        uint32_t command_buffer_index) {

  struct vulkan_command_buffer_context *command_buffer_ctx =
    ctx->command_buffers + command_buffer_index;

  // Free old descriptor set
  VkResult error = VK_SUCCESS;
  if (command_buffer_ctx->active_descriptor_set)
    error = vkFreeDescriptorSets(ctx->device,
                                 ctx->descriptor_pools[command_buffer_index/ALLOC_STEP],
                                 1,
                                 &command_buffer_ctx->descriptor_set);

  if (error != VK_SUCCESS)
    return error;

  command_buffer_ctx->active_descriptor_set = 1;

  VkDescriptorSetAllocateInfo descriptor_set_allocate_info;
  descriptor_set_allocate_info.sType              = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  descriptor_set_allocate_info.pNext              = 0;
  descriptor_set_allocate_info.descriptorPool     = ctx->descriptor_pools[command_buffer_index/ALLOC_STEP];
  descriptor_set_allocate_info.descriptorSetCount = 1;
  descriptor_set_allocate_info.pSetLayouts        = &sh_ctx->descriptor_set_layout;

  return vkAllocateDescriptorSets(ctx->device,
                                  &descriptor_set_allocate_info,
                                  &command_buffer_ctx->descriptor_set);
}

VkResult vulkan_begin_recording(struct vulkan_context *ctx, uint32_t command_buffer_index) {

  VkCommandBuffer command_buffer = ctx->command_buffers[command_buffer_index].command_buffer;

  VkCommandBufferBeginInfo command_buffer_begin_info;
  command_buffer_begin_info.sType            = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  command_buffer_begin_info.pNext            = 0;
  command_buffer_begin_info.flags            = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  command_buffer_begin_info.pInheritanceInfo = 0;

  return vkBeginCommandBuffer(command_buffer, &command_buffer_begin_info);
}

VkResult vulkan_end_recording_and_submit(struct vulkan_context *ctx,
                                         uint32_t command_buffer_index) {

  struct vulkan_command_buffer_context command_buffer_ctx =
    ctx->command_buffers[command_buffer_index];

  VkResult error = vkEndCommandBuffer(command_buffer_ctx.command_buffer);

  if (error != VK_SUCCESS)
    return error;

  VkSubmitInfo submit_info;
  submit_info.sType                = VK_STRUCTURE_TYPE_SUBMIT_INFO;
  submit_info.pNext                = 0;
  submit_info.waitSemaphoreCount   = 0;
  submit_info.pWaitSemaphores      = 0;
  submit_info.pWaitDstStageMask    = 0;
  submit_info.commandBufferCount   = 1;
  submit_info.pCommandBuffers      = &command_buffer_ctx.command_buffer;
  submit_info.signalSemaphoreCount = 0;
  submit_info.pSignalSemaphores    = 0;
  
  return vkQueueSubmit(ctx->queue, 1, &submit_info, command_buffer_ctx.fence);
}

void vulkan_copy_buffers(struct vulkan_context *ctx,
                         VkBufferCopy buffer_copy,
                         struct vk_buffer_mem_pair *src,
                         struct vk_buffer_mem_pair *dest) {

  uint32_t command_buffer_index;
  VULKAN_SUCCEED(vulkan_available_command_buffer(ctx, &command_buffer_index));
  VkCommandBuffer command_buffer = ctx->command_buffers[command_buffer_index].command_buffer;

  vulkan_transfer_ownership(ctx, src, command_buffer_index);
  vulkan_transfer_ownership(ctx, dest, command_buffer_index);

  VULKAN_SUCCEED(vulkan_begin_recording(ctx, command_buffer_index));
  vkCmdCopyBuffer(command_buffer, src->buffer, dest->buffer, 1, &buffer_copy);
  VULKAN_SUCCEED(vulkan_end_recording_and_submit(ctx, command_buffer_index));
}

void vulkan_dispatch(struct vulkan_context *ctx,
                     struct vulkan_shader_context *sh_ctx,
                     uint32_t command_buffer_index,
                     VkPipeline pipeline,
                     uint32_t kernel_x_size,
                     uint32_t kernel_y_size,
                     uint32_t kernel_z_size) {

  VkCommandBuffer command_buffer = ctx->command_buffers[command_buffer_index].command_buffer;
  VkDescriptorSet descriptor_set = ctx->command_buffers[command_buffer_index].descriptor_set;

  VULKAN_SUCCEED(vulkan_begin_recording(ctx, command_buffer_index));
  vkCmdBindPipeline(command_buffer, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline);
  vkCmdBindDescriptorSets(command_buffer, VK_PIPELINE_BIND_POINT_COMPUTE, sh_ctx->pipeline_layout,
                          0, 1, &descriptor_set, 0, 0);
  vkCmdDispatch(command_buffer, kernel_x_size, kernel_y_size, kernel_z_size);
  VULKAN_SUCCEED(vulkan_end_recording_and_submit(ctx, command_buffer_index));
}

static void vulkan_shader_cleanup(struct vulkan_context *ctx,
                                  struct vulkan_shader_context *sh_ctx) {
  vkDestroyPipelineLayout(ctx->device, sh_ctx->pipeline_layout, 0);
  vkDestroyDescriptorSetLayout(ctx->device, sh_ctx->descriptor_set_layout, 0);
  vkDestroyShaderModule(ctx->device, sh_ctx->shader_module, 0);
  for(int i = 0; i < sh_ctx->pipeline_cache_count; ++i) {
    free(sh_ctx->pipeline_cache[i].key);
    vulkan_destroy_pipeline(ctx, sh_ctx->pipeline_cache[i].pipeline);
  }
  if(sh_ctx->pipeline_cache_count > 0)
    free(sh_ctx->pipeline_cache);
}

static void vulkan_cleanup(struct vulkan_context *ctx) {
  vkQueueWaitIdle(ctx->queue);
  vulkan_free_all_scalars(ctx);
  vulkan_free_all(ctx);
  vulkan_free_all_command_buffers(ctx);
  free(ctx->descriptor_pools);
  free(ctx->command_buffers);
  vkDestroyCommandPool(ctx->device, ctx->command_pool, 0);
  vkDestroyDevice(ctx->device, 0);
  vkDestroyInstance(ctx->instance, 0);
}